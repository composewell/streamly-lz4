module Streamly.Compression.LZ4
    ( compressD
    , decompressD
    , compress
    , decompress
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Word (Word8)

import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Streamly.Prelude

--------------------------------------------------------------------------------
-- Compression
--------------------------------------------------------------------------------

-- | See 'compress' for documentation.
compressD ::
       Monad m
    => Int
    -> D.Stream m (A.Array Word8)
    -> D.Stream m (A.Array Word8)
compressD = undefined

-- | Compress a stream of foreign arrays using LZ4 stream compression.
-- @compress i m@ compresses the stream @m@ using the acceleration value @i@.
--
-- As the acceleration increases, the compression speed increases whereas the
-- compression ratio decreases.
--
compress ::
       Monad m => Int -> SerialT m (A.Array Word8) -> SerialT m (A.Array Word8)
compress i m = D.fromStreamD (compressD i (D.toStreamD m))

--------------------------------------------------------------------------------
-- Decompression
--------------------------------------------------------------------------------

-- | See 'decompress' for documentation.
decompressD ::
       Monad m => D.Stream m (A.Array Word8) -> D.Stream m (A.Array Word8)
decompressD = undefined

-- | Decompress a stream of arrays compressed using LZ4 stream compression.
decompress :: Monad m => SerialT m (A.Array Word8) -> SerialT m (A.Array Word8)
decompress m = D.fromStreamD (decompressD (D.toStreamD m))
