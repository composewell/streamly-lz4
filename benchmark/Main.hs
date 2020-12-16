module Main (main) where

import Data.Word (Word8)
import Data.Function ((&))
import System.IO (IOMode(..), openFile, hClose)
import System.Directory (getCurrentDirectory)

import qualified Streamly.Compression.LZ4 as Z
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.FileSystem.Handle as H

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
benchCorpus :: String -> (String -> String) -> Combinator -> Benchmark
benchCorpus n f c =
    bench (f (n ++ " : "))
        $ nfIO
        $ do
            base <- getCurrentDirectory
            let corpora = base ++ "/corpora/"
            h <- openFile (f corpora) ReadMode
            S.unfold H.readChunksWithBufferOf (50, h) & c & S.drain
            hClose h

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE compress #-}
compress :: (String -> String) -> Benchmark
compress f = benchCorpus "compress 5" f (Z.compress 5)

{-# INLINE decompressResizedCompress #-}
decompressResizedCompress :: (String -> String) -> Benchmark
decompressResizedCompress f =
    benchCorpus "decompressResized . compress 5" f
        (Z.decompressResized . Z.compress 5)

{-# INLINE decompressCompress #-}
decompressCompress :: (String -> String) -> Benchmark
decompressCompress f =
    benchCorpus "decompress . compress 5" f (Z.decompress . Z.compress 5)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main =
    defaultMain
        [ bgroup
              "Cantrbry"
              [ compress cantrbry_alice29_txt
              , compress cantrbry_kennedy_xls
              , decompressResizedCompress cantrbry_alice29_txt
              , decompressResizedCompress cantrbry_kennedy_xls
              , decompressCompress cantrbry_alice29_txt
              , decompressCompress cantrbry_kennedy_xls
              ]
        , bgroup
              "Large"
              [ compress large_world192_txt
              , compress large_bible_txt
              , decompressResizedCompress large_world192_txt
              , decompressResizedCompress large_bible_txt
              , decompressCompress large_world192_txt
              , decompressCompress large_bible_txt
              ]
        , bgroup
              "Artificl"
              [ compress artificl_aaa_txt
              , compress artificl_random_txt
              , decompressResizedCompress artificl_aaa_txt
              , decompressResizedCompress artificl_random_txt
              , decompressCompress artificl_aaa_txt
              , decompressCompress artificl_random_txt
              ]
        ]
