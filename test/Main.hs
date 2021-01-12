module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Int (Int32)
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

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.FileSystem.Handle as Handle

import Streamly.Internal.LZ4
import Streamly.LZ4

unsafeIntToI32 :: Int -> Int32
unsafeIntToI32 = fromIntegral

-- Choose only 0 and 1 so that we can get good compressible sequences.
genArrayW8 :: Gen  (Array.Array Word8)
genArrayW8 = Array.fromList <$> listOf (elements [0,1])

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

decompressCompressChunk :: Int -> Array.Array Word8 -> IO ()
decompressCompressChunk i arr = do
    lz4Ctx <- c_createStream
    lz4CtxD <- c_createStreamDecode
    compressed <- compressChunk i lz4Ctx arr
    decompressed <- decompressChunk lz4CtxD compressed
    c_freeStream lz4Ctx
    c_freeStreamDecode lz4CtxD
    decompressed `shouldBe` arr

decompressCompressChunk2 ::
       Int -> Array.Array Word8 -> Array.Array Word8 -> IO ()
decompressCompressChunk2 i arr1 arr2 = do
    lz4Ctx <- c_createStream
    lz4CtxD <- c_createStreamDecode
    compressed1 <- compressChunk i lz4Ctx arr1
    compressed2 <- compressChunk i lz4Ctx arr2
    decompressed1 <- decompressChunk lz4CtxD compressed1
    decompressed2 <- decompressChunk lz4CtxD compressed2
    c_freeStream lz4Ctx
    c_freeStreamDecode lz4CtxD
    (decompressed1, decompressed2) `shouldBe` (arr1, arr2)

decompressResizedcompress :: Int -> [Array.Array Word8] -> IO ()
decompressResizedcompress i lst =
    let strm = Stream.fromList lst
     in do lst1 <- Stream.toList $ decompressResized $ compress i strm
           lst `shouldBe` lst1

    where

    decompressResized = fromStreamD . decompressResizedD . toStreamD

decompressCompress :: Int -> Int -> [Array.Array Word8] -> IO ()
decompressCompress bufsize i lst = do
    let strm = Stream.fromList lst
    withSystemTempFile "LZ4" $ \tmp tmpH -> do
        compress i strm & Handle.fromChunks tmpH
        hClose tmpH
        lst1 <-
            Stream.toList
                $ Stream.bracket_ (openFile tmp ReadMode) hClose
                $ \h ->
                      Stream.unfold Handle.readChunksWithBufferOf (bufsize, h)
                          & decompress
        lst1 `shouldBe` lst

resizeIdempotence :: Property
resizeIdempotence =
    forAll ((,) <$> genAcceleration <*> genArrayW8List)
        $ \(acc, w8List) -> do
              let strm = compress acc $ Stream.fromList w8List
              f1 <- Stream.toList $ resize strm
              f2 <- Stream.toList $ foldr ($) strm $ replicate acc resize
              f1 `shouldBe` f2

    where

    resize = fromStreamD . resizeD . toStreamD

addUncompressedHeader :: Array.Array Word8 -> IO (Array.Array Word8)
addUncompressedHeader arr = do
    let negLen = -1 * unsafeIntToI32 (Array.byteLength arr)
        hdr = Array.fromList [negLen, negLen]
    Array.spliceTwo (coerce hdr) arr

testUncompressedBit :: Int -> [Array.Array Word8] -> Array.Array Word8 -> IO ()
testUncompressedBit acc arrList arr = do
    arrHead <- addUncompressedHeader arr
    l1 <-
        Stream.fromList arrList
            & compress acc
            & Stream.intersperse arrHead
            & decompress
            & Stream.toList
    l2 <- Stream.fromList arrList & Stream.intersperse arr & Stream.toList
    l1 `shouldBe` l2

main :: IO ()
main = do
    large <- generate $ genNonEmptyListWith genArrayW8Large
    largeHC <- generate $ genNonEmptyListWith genArrayW8LargeHC
    uncompBitW8 <- generate genArrayW8
    hspec $ do
        describe "Uncompressed bit tests" $ do
            propsUncompressedBit
            it
                "compress . intersperse . decompress == intersperse (big)"
                (testUncompressedBit 1 largeHC uncompBitW8)
        describe "Idempotence" $
            it "resize" resizeIdempotence
        describe "Identity" $ do
            propsChunk
            propsChunk2
            propsSimple
            forM_ [-1, 5, 12, 100]
                $ \i ->
                      forM_ [1, 512, 32 * 1024, 256 * 1024]
                          $ \bufsize -> do
                                propsBig bufsize i large
                                describe "Highly compressible"
                                    $ propsBig bufsize i largeHC

    where

    propsUncompressedBit = do
        it "compress . intersperse . decompress == intersperse"
            $ property
            $ forAll
                  ((,,) <$> genAcceleration <*> genArrayW8List <*> genArrayW8)
                  (\(a, ls, l) -> testUncompressedBit a ls l)

    propsSimple = do
        it "decompressResized . compress == id"
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8List)
                  (monadicIO . liftIO . uncurry decompressResizedcompress)
        it "decompress . compress == id"
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8List)
                  (monadicIO . liftIO . uncurry (decompressCompress 512))

    propsBig bufsize i l = do
        it ("decompressResized . compress (" ++ show i ++ ") == id (big)")
            $ decompressResizedcompress i l
        it
            ("decompress . compress ("
                 ++ show i ++ "/" ++ show bufsize ++ ") == id (big)")
            $ decompressCompress bufsize i l

    propsChunk2 = do
        it "decompressChunk . compressChunk (x2) == id (big)"
            $ property
            $ forAll
                  ((,,)
                       <$> genAcceleration
                       <*> genArrayW8Large
                       <*> genArrayW8Large)
                  (\(i, arr1, arr2) -> decompressCompressChunk2 i arr1 arr2)

    propsChunk = do
        it "decompressChunk . compressChunk == id (big)"
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8Large)
                  (uncurry decompressCompressChunk)
