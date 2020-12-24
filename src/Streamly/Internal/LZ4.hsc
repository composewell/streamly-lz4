{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Streamly.Internal.LZ4
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides all internal combinators required for 'Streamly.LZ4'.
-- All combinators work with stream of 'A.Array' provided by streamly.
--
-- Most of the time, you'll be working with 'Streamly.LZ4'. This is an internal
-- module and is subject to a lot of change. If you see yourself using functions
-- from this module a lot please raise an issue so we can properly expose those
-- functions.
--
module Streamly.Internal.LZ4
    ( debugD
    , debug
    , compressD
    , resizeD
    , decompressResizedD
    ) where


--------------------------------------------------------------------------------
-- Developer notes
--------------------------------------------------------------------------------

-- ## Fusion plugin
-- The annotations 'Fuse' on the data types don't have have any effect unless
-- fusion-plugin is enabled.
--
-- ## Debugging
-- This module also provides some debugging combinators for inspecting the
-- stream after compression. The debugging only work on streams of resized
-- arrays.

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Word (Word32, Word8)
import Foreign.C (CInt(..), CString)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Marshal (copyBytes, free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import Fusion.Plugin.Types (Fuse (..))
import Streamly.Internal.Control.Exception (assertM)
import Streamly.Prelude (SerialT)

import qualified Streamly.Internal.Data.Array.Storable.Foreign as Array
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as Array
import qualified
    Streamly.Internal.Data.Array.Storable.Foreign.Mut.Types as MArray
import qualified Streamly.Internal.Data.Stream.StreamD as Stream

--------------------------------------------------------------------------------
-- CPP helpers
--------------------------------------------------------------------------------

-- Simple helpers for more informative inline statements.

-- FIXME
-- #define INLINE_EARLY  INLINE [2]
-- #define INLINE_NORMAL INLINE [1]
-- #define INLINE_LATE   INLINE [0]

--------------------------------------------------------------------------------
-- Foreign
--------------------------------------------------------------------------------

#include <lz4.h>

data C_LZ4Stream

foreign import ccall unsafe "lz4.h LZ4_createStream"
    c_createStream :: IO (Ptr C_LZ4Stream)

foreign import ccall unsafe "lz4.h LZ4_freeStream"
    c_freeStream :: Ptr C_LZ4Stream -> IO ()

foreign import ccall unsafe "lz4.h LZ4_compressBound"
    c_compressBound :: CInt -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_compress_fast_continue"
    c_compressFastContinue
        :: Ptr C_LZ4Stream
        -> CString
        -> Ptr Word8
        -> CInt
        -> CInt
        -> CInt
        -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_saveDict"
    c_saveDict :: Ptr C_LZ4Stream -> CString -> CInt -> IO CInt

foreign import ccall unsafe "lz4.h LZ4_decompress_safe_usingDict"
    c_decompressSafeUsingDict
        :: CString -> Ptr Word8 -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

-- | See 'debug' for documentation.
{-# INLINE [1] debugD #-}
-- FIXME: {-# INLINE_NORMAL debugD #-}
debugD :: MonadIO m
    => Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
debugD (Stream.Stream step0 state0) = Stream.Stream step (0 :: Int, state0)

    where

    {-# INLINE putDivider #-}
    putDivider = putStrLn "---------------------------------"

    {-# INLINE debugger #-}
    debugger i arr@(Array.Array fb _) = do
        let len = Array.byteLength arr
        if len <= 8
        then do
            putDivider
            putStrLn $ "Index  : " ++ show i
            putStrLn $ "Length : " ++ show len
            putStrLn "Size info isn't available"
        else withForeignPtr fb
                 $ \b -> do
                       decompressedSize <- peek (castPtr b :: Ptr Word32)
                       compressedSize <-
                           peek (castPtr (b `plusPtr` 4) :: Ptr Word32)
                       putDivider
                       putStrLn $ "Index        : " ++ show i
                       putStrLn $ "Length       : " ++ show len
                       putStrLn $ "Compressed   : " ++ show compressedSize
                       putStrLn $ "Decompressed : " ++ show decompressedSize

    {-# INLINE [0] step #-}
    -- FIXME: {-# INLINE_LATE step #-}
    step gst (i, st) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> do
                liftIO $ debugger i arr
                return $ Stream.Yield arr (i + 1, st1)
            Stream.Skip st1 -> return $ Stream.Skip (i, st1)
            Stream.Stop -> do
                liftIO putDivider
                return Stream.Stop

-- | A simple combinator that prints the index, length, compressed size and
-- decompressed size of each array element to standard output.
--
-- This only works on stream of resized arrays.
--
{-# INLINE debug #-}
debug :: MonadIO m
    => SerialT m (Array.Array Word8) -> SerialT m (Array.Array Word8)
debug m = Stream.fromStreamD (debugD (Stream.toStreamD m))

--------------------------------------------------------------------------------
-- Compression
--------------------------------------------------------------------------------

{-# ANN type CompressState Fuse #-}
data CompressState st ctx dict
    = CompressInit st
    | CompressDo st ctx dict
    | CompressDone ctx dict

-- | See 'compress' for documentation.
{-# INLINE [1] compressD #-}
-- FIXME: {-# INLINE_NORMAL compressD #-}
compressD ::
       MonadIO m
    => Int
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
compressD i0 (Stream.Stream step0 state0) =
    Stream.Stream step (CompressInit state0)

    where

    i = fromIntegral $ max i0 0

    -- Having NOINLINE here does not effect the performance a lot. Every
    -- iteration of the loop is a little slower (< 1us) but the entire loop
    -- fuses.
    -- On a stream with 404739 elements of 10 bytes each,
    -- With NOINLINE: 96.14 ms
    -- With INLINE:   81.07 ms
    --
    -- With INLINE statement and the usage of fusion-plugin results in an
    -- enormous code size when used with other combinators.
    {-# NOINLINE compressChunk #-}
    compressChunk arr lz4Ctx dict = do
        Array.asPtr arr
            $ \src -> do
                  let srcLen = fromIntegral $ Array.byteLength arr
                  maxCLen <- c_compressBound srcLen
                  -- allocate compressed block with 8 byte header
                  (MArray.Array fptr dstBegin dstMax) <-
                        MArray.newArray (fromIntegral maxCLen + 8)
                  let hdrSrcLen :: Ptr Word32 = castPtr dstBegin
                      hdrCompLen :: Ptr Word32 = dstBegin `plusPtr` 4
                      compData = dstBegin `plusPtr` 8
                  compLen <-
                       c_compressFastContinue
                              lz4Ctx src compData srcLen maxCLen i
                  void $ c_saveDict lz4Ctx dict (64 * 1024)
                  poke hdrSrcLen (fromIntegral srcLen)
                  poke hdrCompLen (fromIntegral compLen)
                  let dstEnd = dstBegin `plusPtr` (fromIntegral compLen + 8)
                      compArr = MArray.Array fptr dstEnd dstMax
                  Array.unsafeFreeze <$> MArray.shrinkToFit compArr

    {-# INLINE [0] step #-}
    -- FIXME: {-# INLINE_LATE step #-}
    step _ (CompressInit st) =
        liftIO
            $ do
                lz4Ctx <- c_createStream
                dict <- mallocBytes (64 * 1024) :: IO CString
                return $ Stream.Skip $ CompressDo st lz4Ctx dict
    step gst (CompressDo st lz4Ctx dict) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> do
                arr1 <- liftIO $ compressChunk arr lz4Ctx dict
                return $ Stream.Yield arr1 (CompressDo st1 lz4Ctx dict)
            Stream.Skip st1 ->
                return $ Stream.Skip $ CompressDo st1 lz4Ctx dict
            Stream.Stop -> return $ Stream.Skip $ CompressDone lz4Ctx dict
    step _ (CompressDone lz4Ctx dict) =
        liftIO $ c_freeStream lz4Ctx >> free dict >> return Stream.Stop

--------------------------------------------------------------------------------
-- Decompression
--------------------------------------------------------------------------------

{-# ANN type ResizeState Fuse #-}
data ResizeState st arr
    = RInit st
    | RProcess st arr
    | RAccumlate st arr
    | RYield arr (ResizeState st arr)
    | RDone

-- | See 'resize' for documentation.
--
{-# INLINE [1] resizeD #-}
-- FIXME: {-# INLINE_NORMAL resizeD #-}
resizeD :: MonadIO m => 
    Stream.Stream m (Array.Array Word8) -> Stream.Stream m (Array.Array Word8)
resizeD (Stream.Stream step0 state0) = Stream.Stream step (RInit state0)

    where

    {-# INLINE process #-}
    process st arr@(Array.Array fb e) = do
        let len = Array.byteLength arr
        if len <= 8
        then return $ Stream.Skip $ RAccumlate st arr
        else withForeignPtr fb
                 $ \b -> do
                       compressedSize <-
                           peek (castPtr (b `plusPtr` 4) :: Ptr Word32)
                       let required = fromIntegral compressedSize + 8
                       if len == required
                       then return $ Stream.Skip $ RYield arr $ RInit st
                       else if len < required
                       then return $ Stream.Skip $ RAccumlate st arr
                       else do
                           let arr1E = b `plusPtr` required
                               arr1 = Array.Array fb arr1E
                               arr2S = fb `plusForeignPtr` required
                               arr2 = Array.Array arr2S e
                           return $ Stream.Skip $ RYield arr1 $ RProcess st arr2

    {-# INLINE [0] step #-}
    -- FIXME: {-# INLINE_LATE step #-}
    step _ (RYield r next) = return $ Stream.Yield r next
    step gst (RInit st) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> liftIO $ process st1 arr
            Stream.Skip st1 -> return $ Stream.Skip $ RInit st1
            Stream.Stop -> return Stream.Stop
    step _ (RProcess st arr) = liftIO $ process st arr
    step gst (RAccumlate st buf) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> do
                arr1 <- Array.spliceTwo buf arr
                liftIO $ process st1 arr1
            Stream.Skip st1 -> return $ Stream.Skip $ RAccumlate st1 buf
            Stream.Stop -> return $ Stream.Skip $ RYield buf RDone
    step _ RDone = return Stream.Stop

{-# ANN type DecompressState Fuse #-}
data DecompressState buf st dict dsize
    = DecompressInit st
    | DecompressDo st dict dsize
    | DecompressDone dict

-- | See 'decompressResized' for documentation.
--
{-# INLINE [1] decompressResizedD #-}
-- FIXME: {-# INLINE_NORMAL decompressResizedD #-}
decompressResizedD :: MonadIO m
       => Stream.Stream m (Array.Array Word8)
       -> Stream.Stream m (Array.Array Word8)
decompressResizedD (Stream.Stream step0 state0) =
    Stream.Stream step (DecompressInit state0)

   where

    -- Having NOINLINE here does not effect the performance a lot. Every
    -- iteration of the loop is a little slower (< 1us) but the entire loop
    -- fuses.
    --
    -- With INLINE statement and the usage of fusion-plugin results in an
    -- enormous code size when used with other combinators.
    {-# NOINLINE decompressChunk #-}
    decompressChunk arr dict dsize = do
        Array.asPtr arr
            $ \src -> do
                  let hdrDecompLen :: Ptr Int32 = castPtr src
                      hdrSrcLen :: Ptr Int32 = src `plusPtr` 4
                      compData = src `plusPtr` 8
                  decompLen <- peek hdrDecompLen
                  srcLen <- peek hdrSrcLen
                  (MArray.Array fptr dstBegin dstMax) <-
                        MArray.newArray
                            ((fromIntegral :: Int32 -> Int) decompLen)
                  decompLen1 <-
                      c_decompressSafeUsingDict
                          compData
                          dstBegin
                          (coerce srcLen)
                          (coerce decompLen)
                          dict
                          dsize
                  assertM (decompLen == coerce decompLen1)
                  copyBytes
                      dict dstBegin (min (fromIntegral decompLen1) (64 * 1024))
                  let dstEnd = dstBegin `plusPtr` fromIntegral decompLen1
                      decompArr = MArray.Array fptr dstEnd dstMax
                  decompArr1 <-
                        Array.unsafeFreeze <$> MArray.shrinkToFit decompArr
                  return (decompArr1, decompLen1)

    {-# INLINE [0] step #-}
    -- FIXME: {-# INLINE_LATE step #-}
    step _ (DecompressInit st) =
        liftIO
            $ do
                dict <- mallocBytes (64 * 1024) :: IO (Ptr Word8)
                return $ Stream.Skip $ DecompressDo st dict 0
    step _ (DecompressDone dict) =
        liftIO $ free dict >> return Stream.Stop
    step gst (DecompressDo st dict dsize) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> do
                (arr1, dsize1) <- liftIO $ decompressChunk arr dict dsize
                return $ Stream.Yield arr1 (DecompressDo st1 dict dsize1)
            Stream.Skip st1 ->
                return $ Stream.Skip $ DecompressDo st1 dict dsize
            Stream.Stop ->
                return $ Stream.Skip $ DecompressDone dict
