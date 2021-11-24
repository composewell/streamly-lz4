-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Data.Function ((&))
import Streamly.Internal.Data.Stream.IsStream.Type (fromStreamD, toStreamD)
import System.IO (IOMode(..), openFile, hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Property, forAll, property)
import Test.QuickCheck.Gen
    ( Gen, choose, elements, frequency, generate, listOf, vectorOf )
import Test.QuickCheck.Monadic (monadicIO)

import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.FileSystem.Handle as Handle

import Streamly.Internal.LZ4.Config
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
genArrayW8LargeHC :: (Int, Int) -> Gen (Array.Array Word8)
genArrayW8LargeHC (min_, max_) = do
    arrS <- choose (min_, max_)
    Array.fromList <$> vectorOf arrS (frequency [(9, pure 0), (1, pure 1)])

genNonEmptyListWith :: Gen (Array.Array Word8) -> Gen [Array.Array Word8]
genNonEmptyListWith gen = do
    sz <- choose (50, 100)
    vectorOf sz gen

genAcceleration :: Gen Int
genAcceleration = elements [-1..12]

decompressCompressChunk :: BlockConfig -> Int -> Array.Array Word8 -> IO ()
decompressCompressChunk conf i arr = do
    lz4Ctx <- c_createStream
    lz4CtxD <- c_createStreamDecode
    compressed <- compressChunk conf i lz4Ctx arr
    decompressed <- decompressChunk conf lz4CtxD compressed
    c_freeStream lz4Ctx
    c_freeStreamDecode lz4CtxD
    decompressed `shouldBe` arr

decompressCompressChunk2 ::
       BlockConfig -> Int -> Array.Array Word8 -> Array.Array Word8 -> IO ()
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

decompressResizedcompress :: BlockConfig -> Int -> [Array.Array Word8] -> IO ()
decompressResizedcompress conf i lst =
    let strm = Stream.fromList lst
     in do lst1 <-
               Stream.toList $ decompressChunksRaw $ compressChunks conf i strm
           lst `shouldBe` lst1

    where

    decompressChunksRaw = fromStreamD . decompressChunksRawD conf . toStreamD

decompressCompress :: BlockConfig -> Int -> Int -> [Array.Array Word8] -> IO ()
decompressCompress conf bufsize i lst = do
    let strm = Stream.fromList lst
    withSystemTempFile "LZ4" $ \tmp tmpH -> do
        compressChunks conf i strm & Handle.putChunks tmpH
        hClose tmpH
        lst1 <-
            Stream.toList
                $ Stream.bracket_ (openFile tmp ReadMode) hClose
                $ \h ->
                      Stream.unfold Handle.readChunksWithBufferOf (bufsize, h)
                          & decompressChunks conf
        lst1 `shouldBe` lst

{-# INLINE endMarkArr #-}
endMarkArr :: Array.Array Word8
endMarkArr = Array.fromListN 4 [0,0,0,0]

decompressCompressFrame ::
       BlockConfig -> FrameConfig -> Int -> Int -> [Array.Array Word8] -> IO ()
decompressCompressFrame bf ff bufsize i lst = do
    let strm = Stream.fromList lst
    withSystemTempFile "LZ4" $ \tmp tmpH -> do
        compressChunksFrame strm & Handle.putChunks tmpH
        hClose tmpH
        lst1 <-
            Stream.toList
                $ Stream.bracket_ (openFile tmp ReadMode) hClose
                $ \h ->
                      Stream.unfold Handle.readChunksWithBufferOf (bufsize, h)
                          & decompressChunksFrame
        lst1 `shouldBe` lst

    where

    decompressChunksFrame
        :: Stream.SerialT IO (Array.Array Word8)
        -> Stream.SerialT IO (Array.Array Word8)
    decompressChunksFrame =
        fromStreamD . decompressChunksRawD bf . resizeChunksD bf ff . toStreamD

    compressChunksFrame
        :: Stream.SerialT IO (Array.Array Word8)
        -> Stream.SerialT IO (Array.Array Word8)
    compressChunksFrame strm =
        if hasEndMark ff
        then (fromStreamD . compressChunksD bf i . toStreamD) strm
                 `Stream.append` Stream.fromPure endMarkArr
        else (fromStreamD . compressChunksD bf i . toStreamD) strm

decompressWithCompress :: Int -> Int -> [Array.Array Word8] -> IO ()
decompressWithCompress bufsize i lst = do
    -- Stream with header that returns the config
    -- Magic Little endian (4 bytes) = 407708164
    let magicLE = [4, 34, 77, 24]
    -- flg with everything unset
        flg = 64
    -- bd with 64KB block max size
        bd = 64
        headerChk = 0
        headerList = magicLE ++ [flg, bd, headerChk]
        header = Stream.fromList headerList
    headerArr <- Stream.fold (Array.writeN (length headerList)) header
    (bf, ff) <- Stream.parseD simpleFrameParserD header
    let strm = Stream.fromList lst
    withSystemTempFile "LZ4" $ \tmp tmpH -> do
        compressChunksFrame bf ff i strm
            & Stream.cons headerArr
            & Handle.putChunks tmpH
        hClose tmpH
        lst1 <-
            Stream.toList
                $ Stream.bracket_ (openFile tmp ReadMode) hClose
                $ \h ->
                      Stream.unfold Handle.readChunksWithBufferOf (bufsize, h)
                          & decompressWith_
        lst1 `shouldBe` lst

    where

    compressChunksFrame
        :: BlockConfig
        -> FrameConfig
        -> Int
        -> Stream.SerialT IO (Array.Array Word8)
        -> Stream.SerialT IO (Array.Array Word8)
    compressChunksFrame bf ff i_ strm =
        if hasEndMark ff
        then (fromStreamD . compressChunksD bf i_ . toStreamD) strm
                 `Stream.append` Stream.fromPure endMarkArr
        else (fromStreamD . compressChunksD bf i_ . toStreamD) strm

    decompressWith_ ::
           Stream.SerialT IO (Array.Array Word8)
        -> Stream.SerialT IO (Array.Array Word8)
    decompressWith_ =
        fromStreamD . decompressChunksWithD simpleFrameParserD . toStreamD

resizeIdempotence :: BlockConfig -> Property
resizeIdempotence conf =
    forAll ((,) <$> genAcceleration <*> genArrayW8List)
        $ \(acc, w8List) -> do
              let strm = compressChunks conf acc $ Stream.fromList w8List
              f1 <- Stream.toList $ resizeChunks strm
              f2 <- Stream.toList $ foldr ($) strm $ replicate acc resizeChunks
              f1 `shouldBe` f2

    where

    resizeChunks =
        fromStreamD . resizeChunksD conf defaultFrameConfig . toStreamD

main :: IO ()
main = do
    large <- generate $ genNonEmptyListWith genArrayW8Large
    largeHC <-
        generate
            $ genNonEmptyListWith
            $ genArrayW8LargeHC (1024 * 10, 1024 * 64)
    hspec $ do
        describe "Idempotence" $
            it "resize" (resizeIdempotence defaultBlockConfig)
        describe "Identity" $ do
            propsChunk defaultBlockConfig
            propsChunk2 defaultBlockConfig
            propsSimple defaultBlockConfig
            forM_ [-1, 5, 12, 100]
                $ \i ->
                      forM_ [1, 512, 32 * 1024, 256 * 1024]
                          $ \bufsize -> do
                                propsBig defaultBlockConfig bufsize i large
                                describe "Highly compressible"
                                    $ propsBig
                                          defaultBlockConfig bufsize i largeHC
            describe "setBlockMaxSize BlockMax256KB defaultBlockConfig" $ do
                let config = setBlockMaxSize BlockMax256KB defaultBlockConfig
                propsChunk config
                propsChunk2 config
                propsBig config 512 5 largeHC
            describe "defaultBlockConfig" $ do
                propsSimpleDecompressCompress defaultBlockConfig
                propsBigDecompressCompress defaultBlockConfig 512 5 large
                propsBigDecompressCompress defaultBlockConfig 512 5 largeHC
            describe "setFrameEndMark True defaultFrameConfig" $ do
                let ff = setFrameEndMark True defaultFrameConfig
                propsFrame defaultBlockConfig ff 512 5 large
                propsFrame defaultBlockConfig ff 512 5 largeHC
            describe "setFrameEndMark True . setBlockMaxSize BlockMax256KB"
                $ do
                    let bf = setBlockMaxSize BlockMax256KB defaultBlockConfig
                        ff = setFrameEndMark True defaultFrameConfig
                    propsFrame bf ff 512 5 large
                    propsFrame bf ff 512 5 largeHC
            describe "parse config (decompressWith)" $ do
                propsDecompressWithCompress 512 5 largeHC

    where

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

    propsDecompressWithCompress bufsize i l = do
        it
            "decompressWith . compress == id"
            (decompressWithCompress bufsize i l)

    propsFrame bf ff bufsize i l = do
        it ("decompressFrame . compressFrame (" ++ show i ++ ") == id")
            $ decompressCompressFrame bf ff bufsize i l

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
