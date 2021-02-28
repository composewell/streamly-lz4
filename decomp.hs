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
import qualified Streamly.Internal.Data.Producer.Type as Producer

{-
{-# INLINE decompressFrame #-}
decompressFrame :: String -> IO ()
decompressFrame corpus =
    let str = StreamD.unfold File.readChunksWithBufferOf (1024 * 64, corpus)
        -- prod = Producer.concat Producer.fromStreamD Array.producer
        -- srced = Source.producer prod
        srced = Source.producer Producer.fromStreamD
        unf = LZ4.decompressFrame srced
        -- seed = LZ4.ParsingBody $ Source.source $ Just $ Producer.OuterLoop str
        seed = LZ4.ParsingBody $ Source.source $ Just str
     in Stream.drain $ Stream.unfold (Producer.simplify unf) seed
     -}

{-# INLINE decompressFrame #-}
decompressFrame :: String -> IO ()
decompressFrame corpus =
    let str = StreamD.unfold File.readChunksWithBufferOf (1024 * 64, corpus)
        w8prod = Producer.concat_ Producer.fromStreamD Array.producer
        -- unf = LZ4.decompressFrame (Source.producer Producer.fromStreamD)
        pro = LZ4.decompressFrame (Source.producer w8prod)
     in Stream.drain $ Stream.produce pro (Source.source $ Just str)

-- nix-shell --run 'ghc -O2 -fdicts-strict -fspec-constr-recursive=16 -fmax-worker-args=16 -fplugin Fusion.Plugin -ddump-simpl -ddump-to-file -dsuppress-all Scratch.hs'

main :: IO ()
main = decompressFrame "corpora/large/bible.txt.normalized.compressed.frame"

{-
{-# INLINE _64KB #-}
_64KB :: Int
_64KB = 64 * 1024

main :: IO ()
main = do
    let corpus = "corpora/large/bible.txt.normalized.compressed.frame"
    let frameConf =
            LZ4.defaultConfig & LZ4.removeUncompressedSize (fromIntegral _64KB)
                & LZ4.addEndMark
    Stream.unfold File.readChunksWithBufferOf (1024 * 64, corpus)
        & (LZ4.decompress frameConf)
        & Stream.drain
-}
