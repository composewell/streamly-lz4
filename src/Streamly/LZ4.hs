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
module Streamly.LZ4
    ( compress
    , resize
    , decompressResized
    , decompress
    ) where

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

-- | This combinators resizes arrays to the required length. Every element of
-- the resulting stream will be a proper compressed element with 8 bytes of meta
-- data prefixed to it.
--
{-# INLINE resize #-}
resize :: MonadIO m => SerialT m (Array Word8) -> SerialT m (Array Word8)
resize m = fromStreamD (resizeD (toStreamD m))

-- | This combinator assumes all the arrays in the incoming stream are properly
-- resized.
--
-- This combinator works well with untouched arrays compressed with 'compressD'.
-- A random compressed stream would first need to be resized properly with
-- 'resize'.
--
{-# INLINE decompressResized #-}
decompressResized ::
       MonadIO m => SerialT m (Array Word8) -> SerialT m (Array Word8)
decompressResized m = fromStreamD (decompressResizedD (toStreamD m))

-- | Decompress a stream of arrays compressed using LZ4 stream compression.
{-# INLINE decompress #-}
decompress ::
       MonadIO m => SerialT m (Array Word8) -> SerialT m (Array Word8)
decompress = decompressResized . resize
