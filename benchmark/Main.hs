module Main (main) where

import Data.Word (Word8)
import Data.Function ((&))
import Streamly.Internal.Data.Array.Storable.Foreign (Array)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Prelude (SerialT)
import System.IO (IOMode(..), openFile, hClose)
import System.Directory (getCurrentDirectory)

import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.LZ4 as LZ4
import qualified Streamly.LZ4 as LZ4

import Gauge.Main

--------------------------------------------------------------------------------
-- Corpora helpers
--------------------------------------------------------------------------------

{-# INLINE large_bible_txt #-}
large_bible_txt :: String -> String
large_bible_txt base = base ++ "large/bible.txt"

{-# INLINE large_world192_txt #-}
large_world192_txt :: String -> String
large_world192_txt base = base ++ "large/world192.txt"

{-# INLINE cantrbry_alice29_txt #-}
cantrbry_alice29_txt :: String -> String
cantrbry_alice29_txt base = base ++ "cantrbry/alice29.txt"

{-# INLINE cantrbry_kennedy_xls #-}
cantrbry_kennedy_xls :: String -> String
cantrbry_kennedy_xls base = base ++ "cantrbry/kennedy.xls"

{-# INLINE artificl_random_txt #-}
artificl_random_txt :: String -> String
artificl_random_txt base = base ++ "artificl/random.txt"

{-# INLINE artificl_aaa_txt #-}
artificl_aaa_txt :: String -> String
artificl_aaa_txt base = base ++ "artificl/aaa.txt"

--------------------------------------------------------------------------------
-- Benchmark helpers
--------------------------------------------------------------------------------

type Combinator = SerialT IO (Array Word8) -> SerialT IO (Array Word8)

{-# INLINE benchCorpus #-}
benchCorpus :: Int -> String -> (String -> String) -> Combinator -> Benchmark
benchCorpus bufsize name prepend c =
    bench (prepend (name ++ "/"))
        $ nfIO
        $ do
            base <- getCurrentDirectory
            let corpora = base ++ "/corpora/"
            h <- openFile (prepend corpora) ReadMode
            Stream.unfold Handle.readChunksWithBufferOf (bufsize, h) & c
                & Stream.drain
            hClose h

-- You can compare this directly with LZ4 CLI
{-# INLINE benchCorpusWrite #-}
benchCorpusWrite ::
       Int -> String -> (String -> String) -> Combinator -> Benchmark
benchCorpusWrite bufsize name prepend c =
    bench (prepend (name ++ " : "))
        $ nfIO
        $ do
            base <- getCurrentDirectory
            let corpora = base ++ "/corpora/"
            r <- openFile (prepend corpora) ReadMode
            w <- openFile "/dev/null" WriteMode
            Stream.unfold Handle.readChunksWithBufferOf (bufsize, r) & c
                & Handle.fromChunks w
            hClose r
            hClose w

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE compress #-}
compress :: Int -> Int -> (String -> String) -> Benchmark
compress bufsize i prepend =
    benchCorpus bufsize ("compress " ++ show bufsize) prepend (LZ4.compress i)

{-# INLINE compressWrite #-}
compressWrite :: Int -> Int -> (String -> String) -> Benchmark
compressWrite bufsize i prepend =
    benchCorpusWrite
        bufsize
        ("compress (read & write) " ++ show bufsize)
        prepend
        (LZ4.compress i)

{-# INLINE decompressResizedCompress #-}
decompressResizedCompress :: Int -> Int -> (String -> String) -> Benchmark
decompressResizedCompress bufsize i prepend =
    benchCorpus
        bufsize
        ("decompressResized64 . compress " ++ show bufsize)
        prepend
        (decompressResized . LZ4.compress i)

    where

    decompressResized = fromStreamD . LZ4.decompressResizedD . toStreamD

{-# INLINE decompressCompress #-}
decompressCompress :: Int -> Int -> (String -> String) -> Benchmark
decompressCompress bufsize i prepend =
    benchCorpus
        bufsize
        ("decompress64 . compress " ++ show bufsize)
        prepend
        (LZ4.decompress . LZ4.compress i)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [perfGroup 1, perfGroup 12]

    where

    perfGroup i =
        bgroup ("acceleration value == " ++ show i)
            $ do
                func <-
                    [ compress
                    , compressWrite
                    , decompressResizedCompress
                    , decompressCompress
                    ]
                bufsize <- [64, 32000]
                gen <-
                    [ cantrbry_alice29_txt
                    , cantrbry_kennedy_xls
                    , artificl_aaa_txt
                    , artificl_random_txt
                    , large_bible_txt
                    , large_world192_txt
                    ]
                return $ func bufsize i gen
