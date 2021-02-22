module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Data.Function ((&))
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import System.IO (IOMode(..), openFile, hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Property, forAll, property)
import Test.QuickCheck.Gen
    ( Gen, choose, elements, frequency, generate, listOf, vectorOf )
import Test.QuickCheck.Monadic (monadicIO)

import qualified Streamly.Internal.Data.Array.Foreign  as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD
import qualified Streamly.Internal.FileSystem.Handle as Handle

import qualified Streamly.Internal.Data.Producer.Source as Source
import qualified Streamly.Internal.Data.Producer as Producer

import Streamly.Internal.LZ4
import Streamly.LZ4

-- Choose only 0 and 1 so that we can get good compressible sequences.
genArrayW8List :: Gen [Array.Array Word8]
genArrayW8List = listOf $ Array.fromList <$> listOf (elements [0,1])

genArrayW8Large :: Gen (Array.Array Word8)
genArrayW8Large = do
    let minArr = 1024 * 10
        maxArr = 1024 * 100
    arrS <- choose (minArr, maxArr)
    Array.fromList <$> vectorOf arrS (elements [0,1])

-- HC : Highly compressible
genArrayW8LargeHC :: Gen (Array.Array Word8)
genArrayW8LargeHC = do
    let minArr = 1024 * 10
        maxArr = 1024 * 100
    arrS <- choose (minArr, maxArr)
    Array.fromList <$> vectorOf arrS (frequency [(9, pure 0), (1, pure 1)])

genNonEmptyListWith :: Gen (Array.Array Word8) -> Gen [Array.Array Word8]
genNonEmptyListWith gen = do
    sz <- choose (50, 100)
    vectorOf sz gen

genAcceleration :: Gen Int
genAcceleration = elements [-1..12]

decompressCompressChunk :: Config a -> Int -> Array.Array Word8 -> IO ()
decompressCompressChunk conf i arr = do
    lz4Ctx <- c_createStream
    lz4CtxD <- c_createStreamDecode
    compressed <- compressChunk conf i lz4Ctx arr
    decompressed <- decompressChunk conf lz4CtxD compressed
    c_freeStream lz4Ctx
    c_freeStreamDecode lz4CtxD
    decompressed `shouldBe` arr

decompressCompressChunk2 ::
       Config a -> Int -> Array.Array Word8 -> Array.Array Word8 -> IO ()
decompressCompressChunk2 conf i arr1 arr2 = do
    lz4Ctx <- c_createStream
    lz4CtxD <- c_createStreamDecode
    compressed1 <- compressChunk conf i lz4Ctx arr1
    compressed2 <- compressChunk conf i lz4Ctx arr2
    decompressed1 <- decompressChunk conf lz4CtxD compressed1
    decompressed2 <- decompressChunk conf lz4CtxD compressed2
    c_freeStream lz4Ctx
    c_freeStreamDecode lz4CtxD
    (decompressed1, decompressed2) `shouldBe` (arr1, arr2)

decompressResizedcompress :: Config a -> Int -> [Array.Array Word8] -> IO ()
decompressResizedcompress conf i lst =
    let strm = Stream.fromList lst
     in do lst1 <- Stream.toList $ decompressResized $ compress conf i strm
           lst `shouldBe` lst1

    where

    decompressResized =
        fromStreamD . decompressResizedD conf . toStreamD

decompressCompress :: Config a -> Int -> Int -> [Array.Array Word8] -> IO ()
decompressCompress conf bufsize i lst = do
    let strm = Stream.fromList lst
    withSystemTempFile "LZ4" $ \tmp tmpH -> do
        compress conf i strm & Handle.fromChunks tmpH
        hClose tmpH
        lst1 <-
            Stream.toList
                $ Stream.bracket_ (openFile tmp ReadMode) hClose
                $ \h ->
                      Stream.unfold Handle.readChunksWithBufferOf (bufsize, h)
                          & decompress conf
        lst1 `shouldBe` lst

decompressFrameCompress :: Int -> [Array.Array Word8] -> IO ()
decompressFrameCompress i lst = do
    let conf = defaultConfig & removeUncompressedSize (1024 * 100) & addEndMark
        strm = compressD conf i $ StreamD.fromList lst
        prod = Producer.concat Producer.fromStreamD Array.producer
        srced = Source.producer prod
        unf = Producer.simplify $ decompressFrame srced
        seed = Source.source $ Just (Producer.OuterLoop strm)
    lst1 <- Stream.toList $ Stream.unfold unf $ ParsingHeader seed
    lst `shouldBe` lst1

resizeIdempotence :: Config a -> Property
resizeIdempotence conf  =
    forAll ((,) <$> genAcceleration <*> genArrayW8List)
        $ \(acc, w8List) -> do
              let strm = compress conf acc $ Stream.fromList w8List
              f1 <- Stream.toList $ resize strm
              f2 <- Stream.toList $ foldr ($) strm $ replicate acc resize
              f1 `shouldBe` f2

    where

    resize = fromStreamD . resizeD conf . toStreamD

main :: IO ()
main = do
    large <- generate $ genNonEmptyListWith genArrayW8Large
    largeHC <- generate $ genNonEmptyListWith genArrayW8LargeHC
    hspec $ do
        describe "Idempotence" $
            it "resize" (resizeIdempotence defaultConfig)
        describe "Identity" $ do
            propsChunk defaultConfig
            propsChunk2 defaultConfig
            propsSimple defaultConfig
            forM_ [-1, 5, 12, 100]
                $ \i ->
                      forM_ [1, 512, 32 * 1024, 256 * 1024]
                          $ \bufsize -> do
                                propsBig defaultConfig bufsize i large
                                describe "Highly compressible"
                                    $ propsBig defaultConfig bufsize i largeHC
            describe "removeUncompressedSize (1024 * 100) defaultConfig" $ do
                let config = removeUncompressedSize (1024 * 100) defaultConfig
                propsChunk config
                propsChunk2 config
                propsBig config 512 5 largeHC
            describe "addEndMark defaultConfig" $ do
                let config = addEndMark defaultConfig
                propsSimpleDecompressCompress config
                propsBigDecompressCompress config 512 5 largeHC
            describe "addEndMark . removeUncompressedSize" $ do
                let config =
                        defaultConfig
                            & removeUncompressedSize (1024 * 100)
                            & addEndMark
                propsSimpleDecompressCompress config
                propsBigDecompressCompress config 512 5 largeHC
            describe "decompressFrame . compress" $ do
                propsDecompressFrameCompress largeHC

    where

    propsDecompressFrameCompress arrList = do
        it  ("decompressFrame . compress == id (big)")
            (decompressFrameCompress 5 arrList)

    propsSimpleDecompressCompress conf = do
        it "decompress . compress == id"
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8List)
                  (monadicIO . liftIO . uncurry (decompressCompress conf 512))

    propsBigDecompressCompress conf bufsize i l = do
        it
            ("decompress . compress ("
                 ++ show i ++ "/" ++ show bufsize ++ ") == id (big)")
            $ decompressCompress conf bufsize i l

    propsSimple conf = do
        it "decompressResized . compress == id"
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8List)
                  (monadicIO . liftIO . uncurry (decompressResizedcompress conf))
        it "decompress . compress == id"
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8List)
                  (monadicIO . liftIO . uncurry (decompressCompress conf 512))

    propsBig conf bufsize i l = do
        it ("decompressResized . compress (" ++ show i ++ ") == id (big)")
            $ decompressResizedcompress conf i l
        it
            ("decompress . compress ("
                 ++ show i ++ "/" ++ show bufsize ++ ") == id (big)")
            $ decompressCompress conf bufsize i l

    propsChunk2 conf = do
        it "decompressChunk . compressChunk (x2) == id (big)"
            $ property
            $ forAll
                  ((,,)
                       <$> genAcceleration
                       <*> genArrayW8Large
                       <*> genArrayW8Large)
                  (\(i, arr1, arr2) -> decompressCompressChunk2 conf i arr1 arr2)

    propsChunk conf = do
        it "decompressChunk . compressChunk == id (big)"
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8Large)
                  (uncurry (decompressCompressChunk conf))
