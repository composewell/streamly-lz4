module Main (main) where

import Control.Monad (unless)
import Data.Semigroup (cycle1)
import Data.Word (Word8)
import Data.Function ((&))
import Streamly.Internal.Data.Array.Storable.Foreign (Array)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Prelude (SerialT)
import System.Directory (getCurrentDirectory, doesFileExist)

import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.LZ4 as LZ4
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

bootstrap :: String -> IO ()
bootstrap file = do
    base <- getCurrentDirectory
    let fp = base ++ "/corpora/" ++ file
        normalizedFp = normalizedName fp
        compressedFpBig = bigCompressedName fp
        compressedFpSmall = smallCompressedName fp
    fileExists <- doesFileExist normalizedFp
    unless fileExists $ do
        putStrLn $ "Normalizing " ++ fp
        let fileStream = Stream.unfold File.read fp
            combinedStream =
                Stream.arraysOf _64KB
                    $ Stream.take _10MB
                    $ cycle1 fileStream
        combinedStream & File.fromChunks normalizedFp
        combinedStream & LZ4.compress 65537 & File.fromChunks compressedFpBig
        combinedStream & LZ4.compress 1 & File.fromChunks compressedFpSmall

--------------------------------------------------------------------------------
-- Benchmark helpers
--------------------------------------------------------------------------------

type Combinator = SerialT IO (Array Word8) -> SerialT IO (Array Word8)

{-# INLINE benchCorpus #-}
benchCorpus :: Int -> String -> String -> Combinator -> Benchmark
benchCorpus bufsize name corpus combinator =
    let bname = ("bufsize(" ++ show bufsize ++ ")/" ++ name ++ "/" ++ corpus)
     in bench bname $ nfIO $ do
            base <- getCurrentDirectory
            let file = base ++ "/corpora/" ++ corpus
            Stream.unfold File.readChunksWithBufferOf (bufsize, file)
                & combinator
                & Stream.drain

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE compress #-}
compress :: Int -> Int -> String -> Benchmark
compress bufsize i corpus =
    benchCorpus bufsize ("compress " ++ show i) corpus (LZ4.compress i)

{-# INLINE decompress #-}
decompress :: Int -> String -> Benchmark
decompress bufsize corpus =
    benchCorpus bufsize "decompress" corpus LZ4.decompress

{-# INLINE resize #-}
resize :: Int -> String -> Benchmark
resize bufsize corpus =
    benchCorpus bufsize "resize" corpus (fromStreamD . LZ4.resizeD . toStreamD)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    bootstrap large_bible_txt
    bootstrap large_world192_txt
    bootstrap cantrbry_alice29_txt
    defaultMain
        [ compression_files
        , decompression_files_big
        , decompression_files_small
        , compression_accelaration
        , compression_buffer
        , decompression_buffer
        , resizing_buffer
        ]

    where

    compression_files =
        bgroup
            "compress/files"
            [ compress _64KB 5 (normalizedName large_bible_txt)
            , compress _64KB 5 (normalizedName large_world192_txt)
            , compress _64KB 5 (normalizedName cantrbry_alice29_txt)
            ]

    decompression_files_big =
        bgroup
            "decompress/files/big"
            [ decompress _64KB (bigCompressedName large_bible_txt)
            , decompress _64KB (bigCompressedName large_world192_txt)
            , decompress _64KB (bigCompressedName cantrbry_alice29_txt)
            ]

    decompression_files_small =
        bgroup
            "decompression/files/small"
            [ decompress _64KB (smallCompressedName large_bible_txt)
            , decompress _64KB (smallCompressedName large_world192_txt)
            , decompress _64KB (smallCompressedName cantrbry_alice29_txt)
            ]

    compression_accelaration =
        bgroup
            "compression/acceleration"
            [ compress _64KB (-1) (normalizedName large_bible_txt)
            , compress _64KB 10 (normalizedName large_bible_txt)
            , compress _64KB 1000  (normalizedName large_bible_txt)
            , compress _64KB 65537 (normalizedName large_bible_txt)
            ]

    compression_buffer =
        bgroup
            "compression/buffer"
            [ compress (_64KB `div` 10) 5 (normalizedName large_bible_txt)
            , compress _64KB 5 (normalizedName large_bible_txt)
            , compress (_64KB * 10) 5 (normalizedName large_bible_txt)
            ]

    decompression_buffer =
        bgroup
            "decompression/buffer"
            [ decompress (_64KB `div` 10) (bigCompressedName large_bible_txt)
            , decompress _64KB (bigCompressedName large_bible_txt)
            , decompress (_64KB * 10) (bigCompressedName large_bible_txt)
            ]

    resizing_buffer =
        bgroup
            "resizing/buffer"
            [ resize (_64KB `div` 10) (bigCompressedName large_bible_txt)
            , resize _64KB (bigCompressedName large_bible_txt)
            , resize (_64KB * 10) (bigCompressedName large_bible_txt)
            ]
