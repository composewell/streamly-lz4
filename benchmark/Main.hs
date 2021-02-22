module Main (main) where

import Control.Monad (unless)
import Data.Semigroup (cycle1)
import Data.Word (Word8)
import Data.Function ((&))
import Streamly.Internal.Data.Array.Foreign (Array)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Prelude (SerialT)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (lookupEnv)

import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.LZ4 as LZ4
import qualified Streamly.LZ4 as LZ4

import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Producer.Source as Source
import qualified Streamly.Internal.Data.Producer as Producer

import Gauge.Main

{-# INLINE _64KB #-}
_64KB :: Int
_64KB = 64 * 1024

{-# INLINE _10MB #-}
_10MB :: Int
_10MB = 10 * 1024 * 1024

--------------------------------------------------------------------------------
-- Corpora helpers
--------------------------------------------------------------------------------

{-# INLINE large_bible_txt #-}
large_bible_txt :: String
large_bible_txt = "large/bible.txt"

{-# INLINE large_world192_txt #-}
large_world192_txt :: String
large_world192_txt = "large/world192.txt"

{-# INLINE cantrbry_alice29_txt #-}
cantrbry_alice29_txt :: String
cantrbry_alice29_txt = "cantrbry/alice29.txt"

--------------------------------------------------------------------------------
-- Bootstrapping
--------------------------------------------------------------------------------

normalizedName :: String -> String
normalizedName x = x ++ ".normalized"

bigCompressedName :: String -> String
bigCompressedName x = x ++ ".normalized.compressed.big"

frameCompressedName :: String -> String
frameCompressedName x = x ++ ".normalized.compressed.frame"

smallCompressedName :: String -> String
smallCompressedName x = x ++ ".normalized.compressed.small"

bootstrap :: String -> IO ()
bootstrap fp = do
    let normalizedFp = normalizedName fp
        compressedFpBig = bigCompressedName fp
        compressedFpSmall = smallCompressedName fp
        compressedFpFrame = frameCompressedName fp
    fileExists <- doesFileExist normalizedFp
    unless fileExists $ do
        putStrLn $ "Normalizing " ++ fp
        let fileStream = Stream.unfold File.read fp
            combinedStream =
                Stream.arraysOf _64KB
                    $ Stream.take _10MB
                    $ cycle1 fileStream
        combinedStream & File.fromChunks normalizedFp
        combinedStream & LZ4.compress LZ4.defaultConfig 65537
                       & File.fromChunks compressedFpBig
        combinedStream & LZ4.compress LZ4.defaultConfig 1
                       & File.fromChunks compressedFpSmall
        let frameConf = LZ4.defaultConfig
                            & LZ4.removeUncompressedSize (fromIntegral _64KB)
                            & LZ4.addEndMark
        combinedStream & LZ4.compress frameConf 65537
                       & File.fromChunks compressedFpFrame

--------------------------------------------------------------------------------
-- Benchmark helpers
--------------------------------------------------------------------------------

type Combinator = SerialT IO (Array Word8) -> SerialT IO (Array Word8)

{-# INLINE benchCorpus #-}
benchCorpus :: Int -> String -> String -> Combinator -> Benchmark
benchCorpus bufsize name corpus combinator =
    let bname = ("bufsize(" ++ show bufsize ++ ")/" ++ name ++ "/" ++ corpus)
     in bench bname $ nfIO $ do
            Stream.unfold File.readChunksWithBufferOf (bufsize, corpus)
                & combinator
                & Stream.drain

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE compress #-}
compress :: Int -> Int -> String -> Benchmark
compress bufsize i corpus =
    benchCorpus bufsize ("compress " ++ show i) corpus
        $ LZ4.compress LZ4.defaultConfig i

{-# INLINE decompress #-}
decompress :: Int -> String -> Benchmark
decompress bufsize corpus =
    benchCorpus bufsize "decompress" corpus
        $ LZ4.decompress LZ4.defaultConfig

{-# INLINE resize #-}
resize :: Int -> String -> Benchmark
resize bufsize corpus =
    benchCorpus bufsize "resize" corpus
        $ fromStreamD . LZ4.resizeD LZ4.defaultConfig . toStreamD

{-# INLINE decompressFrame #-}
decompressFrame :: String -> Benchmark
decompressFrame corpus =
    let str = StreamD.unfold File.readChunksWithBufferOf (_64KB, corpus)
        prod = Producer.concat Producer.fromStreamD Array.producer
        srced = Source.producer prod
        unf = Producer.simplify $ LZ4.decompressFrame srced
        seed = LZ4.ParsingHeader $ Source.source $ Just $ Producer.OuterLoop str
        bname = ("bufsize(" ++ show _64KB ++ ")/decompressFrame/" ++ corpus)
     in bench bname $ nfIO $ Stream.drain $ Stream.unfold unf seed

--------------------------------------------------------------------------------
-- Reading environment
--------------------------------------------------------------------------------

-- Environment variables looked up
-- BENCH_STREAMLY_LZ4_FILE
-- BENCH_STREAMLY_LZ4_STRATEGY
-- - c+speed+buffer
-- - d+buffer
-- - r+buffer
--
-- Example:
-- > export BENCH_STREAMLY_LZ4_FILE="path/to/file/"
-- > export BENCH_STREAMLY_LZ4_STRATEGY="c+400+640000"
-- > cabal bench
--
-- The above commands will benchmark file on compression with acceleration value
-- of 400 and buffer size of 640000

data Strategy
    = Compress Int Int
    | Decompress Int
    | Resize Int

{-# INLINE parseStrategy #-}
parseStrategy :: String -> Strategy
parseStrategy ('c':_:r) =
    let (speed, pbufsize) = span (/= '+') r
     in Compress (read speed) (read (tail pbufsize))
parseStrategy ('d':_:r) = Decompress (read r)
parseStrategy ('r':_:r) = Resize (read r)
parseStrategy _ = error "Cannot parse BENCH_STREAMLY_LZ4_STRATEGY"

{-# INLINE runStrategy #-}
runStrategy :: String -> Strategy -> Benchmark
runStrategy file (Compress speed bufsize) = compress bufsize speed file
runStrategy file (Decompress bufsize) = decompress bufsize file
runStrategy file (Resize bufsize) = resize bufsize file

{-# INLINE tryBenchExternal #-}
tryBenchExternal :: IO (Maybe Benchmark)
tryBenchExternal = do
    fr <- lookupEnv "BENCH_STREAMLY_LZ4_FILE"
    case fr of
        Nothing -> return Nothing
        Just file -> do
            fs <- lookupEnv "BENCH_STREAMLY_LZ4_STRATEGY"
            return
                $ case fs of
                      Nothing -> Nothing
                      Just s -> Just $ runStrategy file (parseStrategy s)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    base <- flip (++) "/corpora/" <$> getCurrentDirectory
    let rel f = base ++ f
        relNormalized f = normalizedName (base ++ f)
        relBigCompressed f = bigCompressedName (base ++ f)
        relSmallCompressed f = smallCompressedName (base ++ f)
        relFrameCompressed f = frameCompressedName (base ++ f)
    bootstrap (rel large_bible_txt)
    bootstrap (rel large_world192_txt)
    bootstrap (rel cantrbry_alice29_txt)
    external <- maybe [] (: []) <$> tryBenchExternal
    defaultMain $
        external ++
        [ compression_files relNormalized
        , decompression_files_big relBigCompressed
        , decompressFrame_files relFrameCompressed
        , decompression_files_small relSmallCompressed
        , compression_accelaration relNormalized
        , compression_buffer relNormalized
        , decompression_buffer relBigCompressed
        , resizing_buffer relBigCompressed
        ]

    where

    compression_files f =
        bgroup
            "compress/files"
            [ compress _64KB 5 (f large_bible_txt)
            , compress _64KB 5 (f large_world192_txt)
            , compress _64KB 5 (f cantrbry_alice29_txt)
            ]

    decompression_files_big f =
        bgroup
            "decompress/files/big"
            [ decompress _64KB (f large_bible_txt)
            , decompress _64KB (f large_world192_txt)
            , decompress _64KB (f cantrbry_alice29_txt)
            ]

    decompressFrame_files f =
        bgroup
            "decompressFrame/files"
            [ decompressFrame (f large_bible_txt)
            , decompressFrame (f large_world192_txt)
            , decompressFrame (f cantrbry_alice29_txt)
            ]

    decompression_files_small f =
        bgroup
            "decompression/files/small"
            [ decompress _64KB (f large_bible_txt)
            , decompress _64KB (f large_world192_txt)
            , decompress _64KB (f cantrbry_alice29_txt)
            ]

    compression_accelaration f =
        bgroup
            "compression/acceleration"
            [ compress _64KB (-1) (f large_bible_txt)
            , compress _64KB 10 (f large_bible_txt)
            , compress _64KB 1000  (f large_bible_txt)
            , compress _64KB 65537 (f large_bible_txt)
            ]

    compression_buffer f =
        bgroup
            "compression/buffer"
            [ compress (_64KB `div` 10) 5 (f large_bible_txt)
            , compress _64KB 5 (f large_bible_txt)
            , compress (_64KB * 10) 5 (f large_bible_txt)
            ]

    decompression_buffer f =
        bgroup
            "decompression/buffer"
            [ decompress (_64KB `div` 10) (f large_bible_txt)
            , decompress _64KB (f large_bible_txt)
            , decompress (_64KB * 10) (f large_bible_txt)
            ]

    resizing_buffer f =
        bgroup
            "resizing/buffer"
            [ resize (_64KB `div` 10) (f large_bible_txt)
            , resize _64KB (f large_bible_txt)
            , resize (_64KB * 10) (f large_bible_txt)
            ]
