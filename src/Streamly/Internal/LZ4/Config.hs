{-# LANGUAGE GADTs
  , TypeOperators
  , ConstraintKinds
  , TypeFamilies
  , DataKinds
  , NamedFieldPuns #-}

-- |
-- Module      : Streamly.Internal.LZ4.Config
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.LZ4.Config
    (
    -- * Configuration
      endMark
    , endMarkArr
    , FrameFormat(..)
    , defaultFrameFormat
    , footerSize
    , validateFooter
    , BlockFormat(..)
    , defaultBlockFormat
    , BlockSize(..)
    , metaSize
    , setUncompSize
    , getUncompSize
    , dataOffset
    , compSizeOffset
    )

where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)

import qualified Streamly.Internal.Data.Array.Foreign as Array

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINE endMark #-}
endMark :: Int32
endMark = 0

{-# INLINE endMarkArr #-}
endMarkArr :: Array.Array Word8
endMarkArr = coerce $ Array.fromListN 1 [endMark]

--------------------------------------------------------------------------------
-- Frame Configuration
--------------------------------------------------------------------------------

-- XXX We need to separate the BlockFormat and FrameFormat configuration. The
-- block level combinators need to accept the block format configuration,
-- whereas the frame parsing combinators need to take frame format config.
-- The block format config would be dynamic depending on the frame format.
-- This also means that block format cannot be static.

-- This type will be used for frame generation and parsing.
data FrameFormat =
    FrameFormat
        { hasEndMark :: Bool
        }

footerSize :: FrameFormat -> Int
footerSize FrameFormat {hasEndMark} =
    if hasEndMark
    then 4
    else 0

validateFooter :: FrameFormat -> Array.Array Word8 -> IO Bool
validateFooter _ _ = return True

-- By default the frame format is defined as follows:
-- * hasContentSize is False
-- * hasDictionaryId is False
-- * hasEndMark is True
-- * hasContentChecksum is False
--
-- The format setter functions can be used to modify the format as desired.

defaultFrameFormat :: FrameFormat
defaultFrameFormat = FrameFormat {hasEndMark = True}

{-

-- Header fields

-- | Whether the frame header has a content size field.
hasContentSize :: Bool -> FrameFormat -> FrameFormat
hasContentSize = undefined

-- | Whether the frame header has a dictionary ID field.
hasDictionaryId :: Bool -> FrameFormat -> FrameFormat
hasDictionaryId = undefined

-- Footer fields

-- | Whether the frame footer has an end mark.
hasEndMark :: Bool -> FrameFormat -> FrameFormat
hasEndMark = undefined

-- | Whether the frame footer has a content checksum after the end mark. If it
-- is True then it implicitly indicates that hasEndMark is set to True.
hasContentChecksum :: Bool -> FrameFormat -> FrameFormat
hasContentChecksum = undefined

-}

--------------------------------------------------------------------------------
-- Block Configuration
--------------------------------------------------------------------------------

-- This type is to be used in decompress/compress chunks APIs. It will be
-- determined by parsing the frame.
data BlockFormat =
    BlockFormat
        { blockSize :: BlockSize
        }

-- By default the block format is defined as follows:
-- * BlockSize is BlockHasSize
-- * Block dependence is False
-- * Block checksum is False
--
-- The format setter functions can be used to modify the format as desired.

defaultBlockFormat :: BlockFormat
defaultBlockFormat = BlockFormat {blockSize = BlockHasSize}

-- | Maximum uncompressed size of a data block.
data BlockSize =
      BlockHasSize -- ^ Block header has uncompressed size after the compressed
                   -- size field. Please note that this option is not in the
                   -- LZ4 specification.
    | BlockMax64KB
    | BlockMax256KB
    | BlockMax1MB
    | BlockMax4MB

metaSize :: BlockFormat -> Int
metaSize BlockFormat {blockSize} =
    case blockSize of
        BlockHasSize -> 8
        _ -> 4

setUncompSize :: BlockFormat -> Ptr Word8 -> Int32 -> IO ()
setUncompSize BlockFormat {blockSize} =
    case blockSize of
        BlockHasSize -> \src -> poke (castPtr src `plusPtr` 4)
        _ -> \_ _ -> return ()

getUncompSize :: BlockFormat -> Ptr Word8 -> IO Int32
getUncompSize BlockFormat {blockSize} =
    case blockSize of
        BlockHasSize -> \src -> peek (castPtr src `plusPtr` 4 :: Ptr Int32)
        BlockMax64KB -> \_ -> return $ 64 * 1024
        BlockMax256KB -> \_ -> return $ 256 * 1024
        BlockMax1MB -> \_ -> return $ 1024 * 1024
        BlockMax4MB -> \_ -> return $ 4 * 1024 * 1024

dataOffset :: BlockFormat -> Int
dataOffset BlockFormat {blockSize} =
    case blockSize of
        BlockHasSize -> 8
        _ -> 4

compSizeOffset :: BlockFormat -> Int
compSizeOffset _ = 0

{-

-- | Set the maximum uncompressed size of the data block.
setBlockMaxSize :: BlockSize -> BlockFormat -> BlockFormat
setBlockMaxSize = undefined

-- | When set, future blocks may depend on the past blocks. Block dependency
-- improves compression ratio, especially for small blocks. On the other hand,
-- it makes random access or multi-threaded decoding impossible.
setBlockDependence :: Bool -> BlockFormat -> BlockFormat
setBlockDependence = undefined

-- | Indicate whether blocks are followed by a 4-byte checksum field.
setBlockChecksum :: Bool -> BlockFormat -> BlockFormat
setBlockChecksum = undefined

-}
