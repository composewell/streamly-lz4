-- |
-- Module      : Streamly.LZ4
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides streamly combinators that perform LZ4
-- compression. Please take a look at <https://github.com/lz4/lz4> to learn
-- more. All combinators work with stream of 'Array' provided by streamly.
--
-- The important combinators are 'compress' and 'decompress'. Their behaviour is
-- straightforward.
--
module Streamly.LZ4
    ( compress
    , decompress
    )

where

--------------------------------------------------------------------------------
-- Developer notes
--------------------------------------------------------------------------------

-- If you look at LZ4 as a black box, the idea behind this library is very
-- simple.
--
-- During compression, all array elements are compressed and prefixed with 8
-- bytes of meta data. 4 bytes for the compressed size 4 bytes for the
-- decompressed.
--
-- If the compressed stream is ever saved to file then this meta data behaves
-- like array boundaries. 'resize' uses this meta data to create a stream that
-- respects these boundaries.
--
-- During decompression, the meta data is looked up and the compressed data used
-- for decompression.

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Storable.Foreign (Array)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Prelude (SerialT)

import Streamly.Internal.LZ4

--------------------------------------------------------------------------------
-- Compression
--------------------------------------------------------------------------------

-- | Compress a stream of foreign arrays (< 2GB / element) using LZ4 stream
-- compression.
--
-- @compress i m@ compresses the stream @m@ using the acceleration value @i@.
-- Ideally, i should be between 1 and 12.
--
-- As the acceleration increases, the compression speed increases whereas the
-- compression ratio decreases.
--
-- The foreign array has to be < 2GB as the compression primitives use 32-bit
-- signed int (CInt) to represent the length of the array. The maximum value of
-- a 32-bit signed int is 2GB. In fact it is a limitation of the C library as
-- the foreign function uses 32-bit signed integers.
{-# INLINE compress #-}
compress ::
       MonadIO m
    => Int
    -> SerialT m (Array Word8)
    -> SerialT m (Array Word8)
compress i m = fromStreamD (compressD i (toStreamD m))

--------------------------------------------------------------------------------
-- Decompression
--------------------------------------------------------------------------------

-- | Decompress a stream of arrays compressed using LZ4 stream compression.
{-# INLINE decompress #-}
decompress ::
       MonadIO m => SerialT m (Array Word8) -> SerialT m (Array Word8)
decompress = fromStreamD . decompressResizedD . resizeD . toStreamD
