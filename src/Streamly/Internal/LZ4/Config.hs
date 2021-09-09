-- |
-- Module      : Streamly.Internal.LZ4.Config
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Frame and block configuration settings for LZ4.
--
module Streamly.Internal.LZ4.Config
    (
    -- * LZ4 Frame Format
    -- | Configuration for a frame consisting of a series of blocks.
      FrameConfig(..)
    , setFrameContentSize
    , setFrameContentChecksum
    , setFrameDictionaryId
    , setFrameBlockIndependence
    , setFrameBlockChecksum
    , setFrameEndMark
    , defaultFrameConfig

    -- * LZ4 Block Format
    -- | Configuration for a single compressed block.
    , BlockConfig(..)
    , BlockSize(..)
    , setBlockMaxSize
    , setBlockIndependence
    , setBlockChecksum
    , defaultBlockConfig
    )

where

--------------------------------------------------------------------------------
-- Frame Configuration
--------------------------------------------------------------------------------

-- | Defines the LZ4 frame format.
newtype FrameConfig =
    FrameConfig
        { hasEndMark :: Bool
        }

-- Header fields

-- | Whether the frame header has a content size field.
--
-- /Unimplemented/
setFrameContentSize :: Bool -> FrameConfig -> FrameConfig
setFrameContentSize = undefined

-- | Whether the frame header has a dictionary ID field.
--
-- /Unimplemented/
setFrameDictionaryId :: Bool -> FrameConfig -> FrameConfig
setFrameDictionaryId = undefined

-- | When 'False', future blocks in the frame may depend on the past blocks.
-- Block dependency improves compression ratio, especially for small blocks. On
-- the other hand, it makes random access or multi-threaded decoding
-- impossible.
--
-- /Unimplemented/
setFrameBlockIndependence :: Bool -> FrameConfig -> FrameConfig
setFrameBlockIndependence = undefined

-- | Indicate whether blocks in the frame are followed by a 4-byte checksum
-- field.
--
-- /Unimplemented/
setFrameBlockChecksum :: Bool -> FrameConfig -> FrameConfig
setFrameBlockChecksum = undefined

-- Footer fields

-- | Whether the frame footer has an end mark.
setFrameEndMark :: Bool -> FrameConfig -> FrameConfig
setFrameEndMark val cfg = cfg {hasEndMark = val}

-- | Whether the frame footer has a content checksum after the end mark. If it
-- is True then it implicitly indicates that setEndMark is set to True.
--
-- /Unimplemented/
setFrameContentChecksum :: Bool -> FrameConfig -> FrameConfig
setFrameContentChecksum = undefined

-- XXX setEndMark should be True by default to conform to the standard.
--
-- | The default settings are:
--
-- * 'setFrameContentSize' 'False'
-- * 'setFrameDictionaryId' 'False'
-- * 'setFrameBlockIndependence' 'False'
-- * 'setFrameBlockChecksum' 'False'
-- * 'setFrameEndMark' 'False'
--
defaultFrameConfig :: FrameConfig
defaultFrameConfig = FrameConfig
    { hasEndMark = False
    }

--------------------------------------------------------------------------------
-- Block Configuration
--------------------------------------------------------------------------------

-- | Maximum uncompressed size of a data block.
data BlockSize =
      BlockHasSize -- ^ Block header has uncompressed size after the compressed
                   -- size field. Please note that this option is not in the
                   -- LZ4 specification.
    | BlockMax64KB
    | BlockMax256KB
    | BlockMax1MB
    | BlockMax4MB

-- | Defines the LZ4 compressed block format.
--
-- @
--  ----------------------------------------------------------------------
-- | Compressed length | Uncompressed length | Data | Checksum            |
-- |       (4 byte)    | (4 byte) (optional) |      | (4 byte) (optional) |
--  ----------------------------------------------------------------------
-- @
--
-- Compressed length is the length of the @Data@ field only.  Uncompressed
-- length is present only when the 'setBlockMaxSize' is set to 'BlockHasSize'
-- this is the length of the block when it is uncompressed. Checksum is present
-- when 'setBlockChecksum' is set to 'True'. The 4-byte fields are stored in
-- machine byte order.
--
newtype BlockConfig =
    BlockConfig
        { blockSize :: BlockSize
        }

-- | Set the maximum uncompressed size of the data block.
setBlockMaxSize :: BlockSize -> BlockConfig -> BlockConfig
setBlockMaxSize bs cfg = cfg {blockSize = bs}

-- | When 'False', the block may depend on the past blocks.
--
-- /Unimplemented/
setBlockIndependence :: Bool -> BlockConfig -> BlockConfig
setBlockIndependence = undefined

-- | Indicate whether the block is followed by a 4-byte checksum field.
--
-- /Unimplemented/
setBlockChecksum :: Bool -> BlockConfig -> BlockConfig
setBlockChecksum = undefined

-- | The default settings are:
--
-- * 'setBlockMaxSize' 'BlockHasSize'
-- * 'setBlockIndependence' 'False'
-- * 'setBlockChecksum' 'False'
--
defaultBlockConfig :: BlockConfig
defaultBlockConfig = BlockConfig {blockSize = BlockHasSize}
