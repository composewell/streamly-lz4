-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Main (main) where

import Control.Monad (unless)
import Data.Function ((&))
import Data.Semigroup (cycle1)
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (lookupEnv)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as Stream (parseD)
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.LZ4 as LZ4
import qualified Streamly.Internal.LZ4.Config as LZ4
import qualified Streamly.LZ4 as LZ4

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

smallCompressedName :: String -> String
smallCompressedName x = x ++ ".normalized.compressed.small"

withCompressedName :: String -> String
withCompressedName x = x ++ ".normalized.compressed.frame"

bootstrap :: String -> IO ()
bootstrap fp = do
    let normalizedFp = normalizedName fp
        compressedFpBig = bigCompressedName fp
        compressedFpSmall = smallCompressedName fp
        compressedWith = withCompressedName fp
    fileExists <- doesFileExist normalizedFp
    unless fileExists $ do
        putStrLn $ "Normalizing " ++ fp
        let fileStream = Stream.unfold File.reader fp
            combinedStream =
                Stream.foldMany (Array.writeN _64KB)
                    $ Stream.take _10MB
                    $ cycle1 fileStream
        combinedStream & File.fromChunks normalizedFp
        combinedStream & LZ4.compressChunks LZ4.defaultBlockConfig 65537
                       & File.fromChunks compressedFpBig
        combinedStream & LZ4.compressChunks LZ4.defaultBlockConfig 1
                       & File.fromChunks compressedFpSmall
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
        x0 <- Stream.parseD LZ4.simpleFrameParserD header
        case x0 of
            Right (bf, ff) ->
                combinedStream & compressChunksFrame bf ff 65537
                            & Stream.cons headerArr
                            & File.fromChunks compressedWith
            Left _ -> return ()

    where

    {-# INLINE endMarkArr #-}
    endMarkArr :: Array.Array Word8
    endMarkArr = Array.fromListN 4 [0,0,0,0]

    compressChunksFrame
        :: LZ4.BlockConfig
        -> LZ4.FrameConfig
        -> Int
        -> Stream IO (Array.Array Word8)
        -> Stream IO (Array.Array Word8)
    compressChunksFrame bf ff i_ strm =
        if LZ4.hasEndMark ff
        then (LZ4.compressChunksD bf i_) strm
                 `Stream.append` Stream.fromPure endMarkArr
        else (LZ4.compressChunksD bf i_) strm


--------------------------------------------------------------------------------
-- Benchmark helpers
--------------------------------------------------------------------------------

type Combinator = Stream IO (Array Word8) -> Stream IO (Array Word8)

{-# INLINE benchCorpus #-}
benchCorpus :: Int -> String -> String -> Combinator -> Benchmark
benchCorpus bufsize name corpus combinator =
    let bname = ("bufsize(" ++ show bufsize ++ ")/" ++ name ++ "/" ++ corpus)
     in bench bname $ nfIO $ do
            Stream.unfold File.chunkReaderWith (bufsize, corpus)
                & combinator
                & Stream.fold Fold.drain

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE compress #-}
compress :: Int -> Int -> String -> Benchmark
compress bufsize i corpus =
    benchCorpus bufsize ("compress " ++ show i) corpus
        $ LZ4.compressChunks LZ4.defaultBlockConfig i

{-# INLINE decompress #-}
decompress :: Int -> String -> Benchmark
decompress bufsize corpus =
    benchCorpus bufsize "decompress" corpus
        $ LZ4.decompressChunks LZ4.defaultBlockConfig

{-# INLINE decompressWith #-}
decompressWith :: Int -> String -> Benchmark
decompressWith bufsize corpus =
    benchCorpus bufsize "decompressWith" corpus
        $ LZ4.decompressChunksWithD LZ4.simpleFrameParserD

{-# INLINE resize #-}
resize :: Int -> String -> Benchmark
resize bufsize corpus =
    benchCorpus bufsize "resize" corpus
        $ LZ4.resizeChunksD LZ4.defaultBlockConfig LZ4.defaultFrameConfig


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
        relWithCompressed f = withCompressedName (base ++ f)
        relSmallCompressed f = smallCompressedName (base ++ f)
    bootstrap (rel large_bible_txt)
    bootstrap (rel large_world192_txt)
    bootstrap (rel cantrbry_alice29_txt)
    external <- maybe [] (: []) <$> tryBenchExternal
    defaultMain $
        external ++
        [ compression_files relNormalized
        , decompression_files_big relBigCompressed
        , decompression_files_with relWithCompressed
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

    decompression_files_with f =
        bgroup
            "decompressWith"
            [ decompressWith _64KB (f large_bible_txt)
            , decompressWith _64KB (f large_world192_txt)
            , decompressWith _64KB (f cantrbry_alice29_txt)
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
