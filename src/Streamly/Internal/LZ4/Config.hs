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
    , defaultConfig
    , addUncompressedSize
    , removeUncompressedSize
    , addChecksum
    , removeChecksum
    )

where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, poke)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

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
    | Checksum

-- | Depending on the configuration the combinators will behave differently. Use
-- the helper functions to set the desired configuration.
data Config a =
    Config
        { metaSize :: Int
        , setUncompSize :: Ptr Word8 -> Int32 -> IO ()
        , getUncompSize :: Ptr Word8 -> IO Int32
        , dataOffset :: Int
        , compSizeOffset :: Int
        }

{-# INLINE defaultConfig #-}
defaultConfig :: Config '[ 'UncompressedSize]
defaultConfig =
    Config
        { metaSize = 8
        , setUncompSize = poke . castPtr
        , getUncompSize = \src -> peek (castPtr src :: Ptr Int32)
        , dataOffset = 8
        , compSizeOffset = 4
        }

-- | Encode uncompressed size along with compressed data block.
addUncompressedSize ::
       (NonMember 'UncompressedSize s)
    => Config s
    -> Config ('UncompressedSize ': s)
addUncompressedSize config =
    config
        { metaSize = metaSize config + 4
        , setUncompSize = poke . castPtr
        , getUncompSize = \src -> peek (castPtr src :: Ptr Int32)
        , dataOffset = dataOffset config + 4
        , compSizeOffset = compSizeOffset config + 4
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
        , compSizeOffset = compSizeOffset config - 4
        }

-- | Encode uncompressed size along with compressed data block.
--
-- /Unimplemented/
addChecksum ::
       (NonMember 'Checksum s)
    => Config s
    -> Config ('Checksum ': s)
addChecksum = error "keepChecksum: Unimplemented"

-- | Don't encode uncompressed size along with compressed data block.
--
-- /Unimplemented/
removeChecksum ::
       (IsMember 'Checksum s)
    => Config s
    -> Config (Delete 'Checksum s)
removeChecksum = error "removeChecksum: Unimplemented"
