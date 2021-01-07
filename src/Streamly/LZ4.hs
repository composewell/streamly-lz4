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

-- | @compress speedup stream@ compresses an input stream of @Array word8@
-- resulting in a stream of @Array Word8@ where each array represents a
-- compressed input block. @speedup@ is a compression speedup factor, the more
-- the value of @speedup@ the faster the compression but the size of compressed
-- data may increase. The factor should be between 1 and 65537 inclusive, if it
-- is less than 1 it is set to 1 if it is more than 65537 then it is set to
-- 65537.
--
-- LZ4 does not allow an uncompressed block of size more than 2,113,929,216
-- (0x7E000000) bytes (a little less than 2GiB).  Each compressed block
-- generated by 'compress' consists of a 8 byte header, the first 4 bytes store
-- the length of uncompressed data and the next 4 bytes the length of the
-- compressed data that follows the header.  If the compressed block length is
-- more than maximum compressed block length (approximately 2GiB) it would
-- result in a decompression error. Currently the 4 byte length fields are
-- stored in machine byte ordering.
--
-- /Since 0.1.0/
{-# INLINE compress #-}
compress ::
       MonadIO m
    => Int
    -> SerialT m (Array Word8)
    -> SerialT m (Array Word8)
compress i m = fromStreamD (compressD defaultConfig i (toStreamD m))

--------------------------------------------------------------------------------
-- Decompression
--------------------------------------------------------------------------------

-- | Decompress a stream of @Array Word8@ compressed using LZ4 stream
-- compression. See 'compress' for the format of the input blocks. The input
-- chunks could be of any size, they are resized to the appropriate block size
-- before decompression based on block headers. The output arrays correspond to
-- one compressed block each.
--
-- /Since 0.1.0/
{-# INLINE decompress #-}
decompress ::
       MonadIO m => SerialT m (Array Word8) -> SerialT m (Array Word8)
decompress =
    fromStreamD
        . decompressResizedD defaultConfig . resizeD defaultConfig . toStreamD
