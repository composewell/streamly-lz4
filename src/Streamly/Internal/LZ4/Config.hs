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
      Config(..)
    , Feature(..)
    , endMark
    , endMarkArr
    , defaultConfig
    , addBlockChecksum
    , removeBlockChecksum
    , addEndMark
    , removeEndMark
    , addFrameChecksum
    , removeFrameChecksum
    , FrameFormat(..)
    , defaultFrameFormat
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
-- Configuration
--------------------------------------------------------------------------------

{-# INLINE endMark #-}
endMark :: Int32
endMark = 0

{-# INLINE endMarkArr #-}
endMarkArr :: Array.Array Word8
endMarkArr = coerce $ Array.fromListN 1 [endMark]

type family Delete x ys where
    Delete a '[] = '[]
    Delete a (a ': ys) = Delete a ys
    Delete a (y ': ys) = y ': Delete a ys

type family Member x ys where
    Member x '[] = 'False
    Member x (x ': ys) = 'True
    Member x (y ': ys) = Member x ys

type NonMember a s = Member a s ~ 'False

type IsMember a s = Member a s ~ 'True

-- | Fields in the LZ4 block or frame that can be configured to be present or
-- absent using 'Config'.
data Feature
    = UncompressedSize -- ^ Use uncompressed size in block headers
    | BlockChecksum
    | FrameChecksum
    | EndMark

-- | Depending on the configuration the combinators will behave differently. Use
-- the helper functions to set the desired configuration.
data Config a =
    Config
        -- depends on end mark, frame checksum
        { hasEndMark :: Bool
        , footerSize :: Int
        , validateFooter :: Array.Array Word8 -> IO Bool
        }

-- XXX We need to separate the BlockFormat and FrameFormat configuration. The
-- block level combinators need to accept the block format configuration,
-- whereas the frame parsing combinators need to take frame format config.
-- The block format config would be dynamic depending on the frame format.
-- This also means that block format cannot be static.

-- This type will be used for frame generation and parsing.
data FrameFormat = FrameFormat {}

-- By default the frame format is defined as follows:
-- * hasContentSize is False
-- * hasDictionaryId is False
-- * hasEndMark is True
-- * hasContentChecksum is False
--
-- The format setter functions can be used to modify the format as desired.

defaultFrameFormat :: FrameFormat
defaultFrameFormat = undefined

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

{-# INLINE defaultConfig #-}
defaultConfig :: Config '[]
defaultConfig =
    Config
        {hasEndMark = False, validateFooter = \_ -> return True, footerSize = 0}

-- | Stop when end mark is reached.
addEndMark :: Config s -> Config ('EndMark ': s)
addEndMark config =
    config {hasEndMark = True, footerSize = footerSize config + 4}

-- | Don't recognize end mark.
removeEndMark ::
       (NonMember 'FrameChecksum s, IsMember 'EndMark s)
    => Config s
    -> Config (Delete 'EndMark s)
removeEndMark config =
    config {hasEndMark = False, footerSize = footerSize config - 4}

-- | Encode block checksum along with compressed data block.
--
-- /Unimplemented/
addBlockChecksum ::
       (NonMember 'BlockChecksum s)
    => Config s
    -> Config ('BlockChecksum ': s)
addBlockChecksum = error "addBlockChecksum: Unimplemented"

-- | Don't encode block checksum along with compressed data block.
--
-- /Unimplemented/
removeBlockChecksum ::
       (IsMember 'BlockChecksum s)
    => Config s
    -> Config (Delete 'BlockChecksum s)
removeBlockChecksum = error "removeBlockChecksum: Unimplemented"

-- | Encode frame checksum after the end mark.
--
-- /Unimplemented/
addFrameChecksum ::
       (NonMember 'FrameChecksum s, IsMember 'EndMark s)
    => Config s
    -> Config ('FrameChecksum ': s)
addFrameChecksum =
    -- This is where we implement a proper validateFooter
    error "addFrameChecksum: Unimplemented"

-- | Don't encode frame checksum after the endmark.
--
-- /Unimplemented/
removeFrameChecksum ::
       (IsMember 'FrameChecksum s)
    => Config s
    -> Config (Delete 'FrameChecksum s)
removeFrameChecksum = error "removeFrameChecksum: Unimplemented"
