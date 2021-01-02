module Main (main) where

import Data.Word (Word8)
import Data.Function ((&))
import System.IO (IOMode(..), openFile, hClose)
import System.Directory (getCurrentDirectory)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.FileSystem.Handle as H
import qualified Streamly.LZ4 as Z

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

type Combinator = S.SerialT IO (A.Array Word8) -> S.SerialT IO (A.Array Word8)

{-# INLINE benchCorpus #-}
benchCorpus :: Int -> String -> (String -> String) -> Combinator -> Benchmark
benchCorpus bufsize name prepend c =
    bench (prepend (name ++ "/"))
        $ nfIO
        $ do
            base <- getCurrentDirectory
            let corpora = base ++ "/corpora/"
            h <- openFile (prepend corpora) ReadMode
            S.unfold H.readChunksWithBufferOf (bufsize, h) & c & S.drain
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
            S.unfold H.readChunksWithBufferOf (bufsize, r) & c & H.fromChunks w
            hClose r
            hClose w

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE compress #-}
compress :: Int -> Int -> (String -> String) -> Benchmark
compress bufsize i prepend =
    benchCorpus bufsize ("compress " ++ show bufsize) prepend (Z.compress i)

{-# INLINE compressWrite #-}
compressWrite :: Int -> Int -> (String -> String) -> Benchmark
compressWrite bufsize i prepend =
    benchCorpusWrite
        bufsize
        ("compress (read & write) " ++ show bufsize)
        prepend
        (Z.compress i)

{-# INLINE decompressResizedCompress #-}
decompressResizedCompress :: Int -> Int -> (String -> String) -> Benchmark
decompressResizedCompress bufsize i prepend =
    benchCorpus
        bufsize
        ("decompressResized64 . compress " ++ show bufsize)
        prepend
        (Z.decompressResized . Z.compress i)

{-# INLINE decompressCompress #-}
decompressCompress :: Int -> Int -> (String -> String) -> Benchmark
decompressCompress bufsize i prepend =
    benchCorpus
        bufsize
        ("decompress64 . compress " ++ show bufsize)
        prepend
        (Z.decompress . Z.compress i)

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
