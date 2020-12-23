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
benchCorpus buf n f c =
    bench (f (n ++ " : "))
        $ nfIO
        $ do
            base <- getCurrentDirectory
            let corpora = base ++ "/corpora/"
            h <- openFile (f corpora) ReadMode
            S.unfold H.readChunksWithBufferOf (buf, h) & c & S.drain
            hClose h

-- You can compare this directly with LZ4 CLI
{-# INLINE benchCorpusWrite #-}
benchCorpusWrite :: Int -> String -> (String -> String) -> Combinator -> Benchmark
benchCorpusWrite buf n f c =
    bench (f (n ++ " : "))
        $ nfIO
        $ do
            base <- getCurrentDirectory
            let corpora = base ++ "/corpora/"
            r <- openFile (f corpora) ReadMode
            w <- openFile "/dev/null" WriteMode
            S.unfold H.readChunksWithBufferOf (buf, r) & c & H.fromChunks w
            hClose r
            hClose w

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE compress #-}
compress :: Int -> Int -> (String -> String) -> Benchmark
compress buf i f = benchCorpus buf ("compress " ++ show buf) f (Z.compress i)

{-# INLINE compressWrite #-}
compressWrite :: Int -> Int -> (String -> String) -> Benchmark
compressWrite buf i f =
    benchCorpusWrite
        buf ("compress (read & write) " ++ show buf) f (Z.compress i)

{-# INLINE decompressResizedCompress #-}
decompressResizedCompress :: Int -> Int -> (String -> String) -> Benchmark
decompressResizedCompress buf i f =
    benchCorpus buf ("decompressResized64 . compress " ++ show buf) f
        (Z.decompressResized . Z.compress i)

{-# INLINE decompressCompress #-}
decompressCompress :: Int -> Int -> (String -> String) -> Benchmark
decompressCompress buf i f =
    benchCorpus buf ("decompress64 . compress " ++ show buf) f
        (Z.decompress . Z.compress i)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [perfGroup 1, perfGroup 12]

    where

    perfGroup i =
        bgroup ("i" ++ show i)
            $ do
                func <-
                    [ compress
                    , compressWrite
                    , decompressResizedCompress
                    , decompressCompress
                    ]
                buf <- [64, 32000]
                gen <-
                    [ cantrbry_alice29_txt
                    , cantrbry_kennedy_xls
                    , artificl_aaa_txt
                    , artificl_random_txt
                    , large_bible_txt
                    , large_world192_txt
                    ]
                return $ func buf i gen
