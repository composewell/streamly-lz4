-- |
-- Module      : Streamly.Internal.LZ4.Frame
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Internal module subject to change without notice.
--
module Streamly.Internal.LZ4.Frame
    (decompressD)

where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (Bits(..))
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.ForeignPtr (plusForeignPtr)
import Foreign.Storable (peek)
import GHC.Ptr (plusPtr)

import qualified Streamly.Internal.Data.Array.Storable.Foreign as Array
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as Array
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD

import Streamly.Internal.LZ4.Config
import Streamly.Internal.LZ4

ntestBit :: Bits a => a -> Int -> Bool
ntestBit a = not . testBit a

data FLG =
    FLG
        { blockIndependence :: Bool
        , blockChecksum :: Bool
        , contentSize :: Bool
        , contentChecksum :: Bool
        , dictID :: Bool
        }

parseFLG :: Word8 -> FLG
parseFLG a =
    if ntestBit a 7 && testBit a 6
    then if ntestBit a 1
    then FLG
             (testBit a 5)
             (testBit a 4)
             (testBit a 3)
             (testBit a 2)
             (testBit a 0)
    else error "parseFLG: Reserved bit is not 0"
    else error "parseFLG: Unsupported version"

parseBD :: Word8 -> Int32
parseBD a =
    case bdValue of
        4 -> 64 * 1024
        5 -> 256 * 1024
        6 -> 1024 * 1024
        7 -> 4 * 1024 * 1024
        _ -> error "parseBD: Unknown block max size"

    where

    bitBow :: Bool -> Int -> Int
    bitBow i j =
        if i
        then 2 ^ j
        else 0

    bdValue =
        if ntestBit a 7
               && ntestBit a 3
               && ntestBit a 2
               && ntestBit a 1
               && ntestBit a 0
        then bitBow (testBit a 6) 2
                 + bitBow (testBit a 5) 1
                 + bitBow (testBit a 4) 0
        else error "parseBD: Reserved bits are not 0"

verifyMinimal :: FLG -> Bool
verifyMinimal FLG {..} =
    not $ blockIndependence
              || blockChecksum
              || contentSize
              || contentChecksum
              || dictID

decompressD ::
       MonadIO m
    => StreamD.Stream m (Array.Array Word8)
    -> StreamD.Stream m (Array.Array Word8)
decompressD s0 = StreamD.concatMap id (StreamD.yieldM action)

    where

    {-# INLINE nextStream #-}
    nextStream arr s = liftIO $ do
        Array.asPtr arr $ \ptr -> do
            flgByte <- peek ptr
            bdByte <- peek (ptr `plusPtr` 1)
            -- Parse the content size, dictID, and header checksum here as
            -- well. We need to create the complete configuration here depending
            -- on flgByte and bdByte.
            let flg = parseFLG flgByte
                size = parseBD bdByte
            -- Since the configuration is minimal, dataOffset is after a
            -- constant [FLG:BD:HC] 3 byte header
            let dataOffset = 3
            if verifyMinimal flg
            then let newStart = Array.aStart arr `plusForeignPtr` dataOffset
                     newArr = Array.Array newStart (Array.aEnd arr)
                     -- XXX StreamD.cons is OK here? Only 1 cons.
                     s1 = newArr `StreamD.cons` s
                     -- Apply proper configuration here
                     config = removeUncompressedSize size defaultConfig
                     s2 = decompressResizedD config (resizeD config s1)
                 in return s2
            else error "decompressD: Unsupported configuration"

    {-# INLINE action #-}
    action = do
        res <- StreamD.uncons s0
        case res of
          Nothing -> error "decompressD: The stream is empty"
          Just (arr, s1) ->
              if Array.byteLength arr >= 3
              then nextStream arr s1
              else actionWith arr s1

    actionWith arr s = do
        res <- StreamD.uncons s
        case res of
          Nothing -> error "decompressD: The stream size is < 3"
          Just (arr1, s1) -> do
              arr2 <- Array.spliceTwo arr arr1
              if Array.byteLength arr2 >= 3
              then nextStream arr s1
              else actionWith arr2 s1
