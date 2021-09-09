-- |
-- Module      : Streamly.LZ4
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streaming APIs for LZ4 (<https://github.com/lz4/lz4>) compression and
-- decompression.
--
-- A compressed LZ4 object (e.g. a file) may be represented by a sequence of
-- one or more LZ4 frames defined by the [LZ4 frame
-- format](https://github.com/lz4/lz4/blob/dev/doc/lz4_Frame_format.md). A
-- frame consists of a frame header followed by a number of compressed blocks
-- and a frame footer. The frame header defines the attributes of the
-- compression method and the blocks in the frame. For example, the blocks may
-- be independently compressed or future blocks may depend on the past blocks.
-- It may also describe the maximum size of the blocks in the frame and use of
-- some optional features.
--
-- This module exposes combinators to only compress or decompress the stream of
-- blocks in a frame and not the frame itself. See the "Streamly.Internal.LZ4"
-- module for an experimental frame parsing function.
--
-- How the blocks are encoded, depends on the attributes specified in the frame
-- header.  We provide a 'BlockConfig' parameter to specify those options when
-- decoding or encoding a stream of blocks. Assuming you have parsed the frame,
-- you can set the 'BlockConfig' accordingly to parse the stream of blocks
-- appropriately.
--
-- Please build with
-- [fusion-plugin](https://hackage.haskell.org/package/fusion-plugin) for best
-- performance. See the [streamly build
-- guide](https://streamly.composewell.com/streamly-0.8.0/Compiling.html) for
-- more details"
--
-- The APIs are not yet stable and may change in future.
--
module Streamly.LZ4
    (
    -- * Configuration
      BlockConfig
    , defaultBlockConfig
    , BlockSize(..)
    , setBlockMaxSize

    -- * Combinators
    , compressChunks
    , decompressChunks
    )

where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Foreign (Array)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Prelude (SerialT)

import Streamly.Internal.LZ4.Config
import Streamly.Internal.LZ4

--------------------------------------------------------------------------------
-- Compression
--------------------------------------------------------------------------------

-- Note that "speedup" is specific to compression, therefore, it is not part of
-- BlockConfig.
--
-- XXX Ensure little-endian byte order as per the spec
-- XXX Check the size of the input array and fail if it is more than the max
-- block size set in BlockConfig.

-- | @compressChunks config speedup stream@ compresses an input stream of
-- @Array word8@ using the configuration defined by @config@. The resulting
-- stream is of type @Array Word8@ where each array represents a compressed
-- input block. Each input array becomes one compressed block.
--
-- @speedup@ is a compression speedup factor, more the value of @speedup@
-- faster the compression but the size of compressed data may increase. The
-- factor should be between 1 and 65537 inclusive, if it is less than 1 it is
-- set to 1 if it is more than 65537 then it is set to 65537.
--
-- LZ4 does not allow an uncompressed block of size more than 2,113,929,216
-- (0x7E000000) bytes (a little less than 2GiB).  If the compressed block
-- length is more than maximum uncompressed block length (approximately 2GiB)
-- it would result in a decompression error.
--
-- See 'BlockConfig' for more details about the format of the compressed block.
--
-- /Since 0.1.0/
{-# INLINE compressChunks #-}
compressChunks ::
       MonadIO m
    => BlockConfig
    -> Int
    -> SerialT m (Array Word8)
    -> SerialT m (Array Word8)
compressChunks cfg i m = fromStreamD (compressChunksD cfg i (toStreamD m))

--------------------------------------------------------------------------------
-- Decompression
--------------------------------------------------------------------------------

-- | Decompress a stream of @Array Word8@ compressed using LZ4 stream
-- compression. See 'compressChunks' for the format of the input blocks. The
-- input chunks could be of any size, they are resized to the appropriate block
-- size before decompression based on block headers. The decompressed output
-- arrays correspond to one compressed block each.
--
-- /Since 0.1.0/
{-# INLINE decompressChunks #-}
decompressChunks ::
       MonadIO m
    => BlockConfig
    -> SerialT m (Array Word8)
    -> SerialT m (Array Word8)
decompressChunks bf =
    fromStreamD
        . decompressChunksRawD bf
        . resizeChunksD bf defaultFrameConfig . toStreamD
