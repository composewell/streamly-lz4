{-# LANGUAGE GADTs, TypeOperators, ConstraintKinds, TypeFamilies, DataKinds #-}

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
    , addUncompressedSize
    , removeUncompressedSize
    , addBlockChecksum
    , removeBlockChecksum
    , addEndMark
    , removeEndMark
    , addFrameChecksum
    , removeFrameChecksum
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

data Feature
    = UncompressedSize
    | BlockChecksum
    | FrameChecksum
    | EndMark

-- | Depending on the configuration the combinators will behave differently. Use
-- the helper functions to set the desired configuration.
data Config a =
    Config
        { metaSize :: Int
        , setUncompSize :: Ptr Word8 -> Int32 -> IO ()
        , getUncompSize :: Ptr Word8 -> IO Int32
        , dataOffset :: Int
        , compSizeOffset :: Int
        , hasEndMark :: Bool
        , validateFooter :: Array.Array Word8 -> IO Bool
        , footerSize :: Int
        }

{-# INLINE defaultConfig #-}
defaultConfig :: Config '[ 'UncompressedSize]
defaultConfig =
    Config
        { metaSize = 8
        , setUncompSize = \src -> poke (castPtr src `plusPtr` 4)
        , getUncompSize = \src -> peek (castPtr src `plusPtr` 4 :: Ptr Int32)
        , dataOffset = 8
        , compSizeOffset = 0
        , hasEndMark = False
        , validateFooter = \_ -> return True
        , footerSize = 0
        }

-- | Encode uncompressed size along with compressed data block.
addUncompressedSize ::
       (NonMember 'UncompressedSize s)
    => Config s
    -> Config ('UncompressedSize ': s)
addUncompressedSize config =
    config
        { metaSize = metaSize config + 4
        , setUncompSize = \src -> poke (castPtr src `plusPtr` 4)
        , getUncompSize = \src -> peek (castPtr src `plusPtr` 4 :: Ptr Int32)
        , dataOffset = dataOffset config + 4
        , compSizeOffset = compSizeOffset config
        }

-- | Don't encode uncompressed size, use @size@ while allocating a new array for
-- decompression instead.
removeUncompressedSize ::
       (IsMember 'UncompressedSize s)
    => Int32
    -> Config s
    -> Config (Delete 'UncompressedSize s)
removeUncompressedSize size config =
    config
        { metaSize = metaSize config - 4
        , setUncompSize = \_ _ -> return ()
        , getUncompSize = \_ -> return size
        , dataOffset = dataOffset config - 4
        , compSizeOffset = compSizeOffset config
        }

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
