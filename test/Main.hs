module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Data.Function ((&))
import System.IO (IOMode(..), openFile, hClose)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (forAll, property)
import Test.QuickCheck.Gen
    ( Gen, choose, chooseAny, elements, generate
    , listOf, vectorOf
    )
import Test.QuickCheck.Monadic (monadicIO)
import Data.List ((\\))

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Memory.ArrayStream as ArrayStream

import Streamly.Internal.LZ4
import Streamly.LZ4

genArrayW8List :: Gen [Array.Array Word8]
genArrayW8List = listOf $ Array.fromList <$> listOf (elements [0,1])

genArrayW8Large :: Gen (Array.Array Word8)
genArrayW8Large = do
    let minArr = 1024 * 10
        maxArr = 1024 * 100
    arrS <- choose (minArr, maxArr)
    Array.fromList <$> vectorOf arrS (elements [0,1])

genArrayW8ListLarge :: Gen [Array.Array Word8]
genArrayW8ListLarge = do
    let minArr = 1024 * 10
        maxArr = 1024 * 100
        minVec = 50
        maxVec = 100
    arrS <- choose (minArr, maxArr)
    vecS <- choose (minVec, maxVec)
    let arrGen = Array.fromList <$> vectorOf arrS (elements [0,1])
    vectorOf vecS arrGen

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

decompressResizedcompress :: (Int, [Array.Array Word8]) -> IO ()
decompressResizedcompress (i, lst) =
    let strm = Stream.fromList lst
     in do
            lst1 <- Stream.toList $ decompressResized $ compress i strm
            putStrLn $ "lst length = " ++ show (length lst)
            putStrLn $ "lst1 length = " ++ show (length lst1)
            if lst /= lst1
            then do
                putStrLn $ show (length $ lst1 \\ lst)
                error "failed"
            else
                return ()

decompressCompress :: Int -> (Int, [Array.Array Word8]) -> IO ()
decompressCompress bufsize (i, lst) = do
    let tmp = "/tmp/test.lz4"
        strm = Stream.fromList lst
    w <- openFile tmp WriteMode
    compress i strm & Handle.fromChunks w
    hClose w
    f1 <-
        Stream.toList
            $ Stream.bracket_ (openFile tmp ReadMode) hClose
            $ \h ->
                  Stream.unfold Handle.readChunksWithBufferOf (bufsize, h)
                & decompress
                & ArrayStream.concat
    f2 <- Stream.toList $ ArrayStream.concat strm
    f1 `shouldBe` f2

main :: IO ()
main = do
    large <- generate genArrayW8ListLarge
    hspec
        $ describe "Identity"
        $ do
              -- propsSimple
              -- propsChunk
              -- propsChunk2
              forM_ [1] $ \i ->
                  forM_ [512] $ \bufsize ->
                      propsBig bufsize (i, large)

    where

    propsSimple = do
        it "decompressResized . compress == id"
            $ property
            $ forAll ((,) <$> genAcceleration <*> genArrayW8List)
            $ monadicIO . liftIO . decompressResizedcompress
        it "decompress . compress == id"
            $ property
            $ forAll ((,) <$> genAcceleration <*> genArrayW8List)
            $ monadicIO . liftIO . decompressCompress 512

    propsBig bufsize r@(i, _) = do
        it ("decompressResized . compress (" ++ show i ++ ") == id (big)")
            $ decompressResizedcompress r
            {-
        it ("decompress . compress (" ++ show i ++ "/" ++ show bufsize ++ ") == id (big)")
                $ decompressCompress bufsize r
                -}

    propsChunk2 = do
        it ("decompressChunk . compressChunk (x2) == id (big)")
            $ property
            $ forAll
                  ((,,)
                       <$> genAcceleration
                       <*> genArrayW8Large
                       <*> genArrayW8Large)
                  (\(i, arr1, arr2) -> decompressCompressChunk2 i arr1 arr2)

    propsChunk = do
        it ("decompressChunk . compressChunk == id (big)")
            $ property
            $ forAll
                  ((,) <$> genAcceleration <*> genArrayW8Large)
                  (\(i, arr) -> decompressCompressChunk i arr)
