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
    , compressChunk
    , decompressChunk
    , compressD
    , resizeD
    , decompressResizedD
    )

where

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

import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Word (Word32, Word8)
import Foreign.C (CInt(..), CString)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
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

data C_LZ4Stream

data C_LZ4StreamDecode

foreign import ccall unsafe "lz4.h LZ4_createStream"
    c_createStream :: IO (Ptr C_LZ4Stream)

foreign import ccall unsafe "lz4.h LZ4_freeStream"
    c_freeStream :: Ptr C_LZ4Stream -> IO ()

foreign import ccall unsafe "lz4.h LZ4_createStreamDecode"
    c_createStreamDecode :: IO (Ptr C_LZ4StreamDecode)

foreign import ccall unsafe "lz4.h LZ4_freeStreamDecode"
    c_freeStreamDecode :: Ptr C_LZ4StreamDecode -> IO ()

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

foreign import ccall unsafe "lz4.h LZ4_decompress_safe_continue"
    c_decompressSafeContinue
        :: Ptr C_LZ4StreamDecode
        -> CString
        -> Ptr Word8
        -> CInt
        -> CInt
        -> IO CInt

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
-- Primitives
--------------------------------------------------------------------------------

-- Having NOINLINE here does not effect the performance a lot. Every
-- iteration of the loop is a little slower (< 1us) but the entire loop
-- fuses.
-- On a stream with 404739 elements of 10 bytes each,
-- With NOINLINE: 96.14 ms
-- With INLINE:   81.07 ms
--
-- With INLINE statement and the usage of fusion-plugin results in an
-- enormous code size when used with other combinators.
-- | Primitive function to compress a chunk of Word8.
{-# NOINLINE compressChunk #-}
compressChunk ::
       Int -> Ptr C_LZ4Stream -> Array.Array Word8 -> IO (Array.Array Word8)
compressChunk i ctx arr = do
    Array.asPtr arr
        $ \src -> do
              let srcLen = fromIntegral $ Array.byteLength arr
              maxCLen <- c_compressBound srcLen
              -- allocate compressed block with 8 byte header.  First 4
              -- bytes of the header store the length of the uncompressed
              -- data and the next 4 bytes store the length of the
              -- compressed data.
              (MArray.Array fptr dstBegin dstMax) <-
                  MArray.newArray (fromIntegral maxCLen + 8)
              let hdrSrcLen :: Ptr Word32 = castPtr dstBegin
                  hdrCompLen :: Ptr Word32 = dstBegin `plusPtr` 4
                  compData = dstBegin `plusPtr` 8
              compLen <-
                  c_compressFastContinue
                      ctx src compData srcLen maxCLen (fromIntegral i)
              poke hdrSrcLen (fromIntegral srcLen)
              poke hdrCompLen (fromIntegral compLen)
              let dstEnd = dstBegin `plusPtr` (fromIntegral compLen + 8)
                  compArr = MArray.Array fptr dstEnd dstMax
              Array.unsafeFreeze <$> MArray.shrinkToFit compArr

-- Having NOINLINE here does not effect the performance a lot. Every
-- iteration of the loop is a little slower (< 1us) but the entire loop
-- fuses.
--
-- With INLINE statement and the usage of fusion-plugin results in an
-- enormous code size when used with other combinators.
-- | Primitive function to decompress a chunk of Word8.
{-# NOINLINE decompressChunk #-}
decompressChunk ::
       Ptr C_LZ4StreamDecode -> Array.Array Word8 -> IO (Array.Array Word8)
decompressChunk ctx arr = do
    Array.asPtr arr
        $ \src -> do
              let hdrDecompLen :: Ptr Int32 = castPtr src
                  hdrSrcLen :: Ptr Int32 = src `plusPtr` 4
                  compData = src `plusPtr` 8
              decompLen <- peek hdrDecompLen
              srcLen <- peek hdrSrcLen
              (MArray.Array fptr dstBegin dstMax) <-
                  MArray.newArray ((fromIntegral :: Int32 -> Int) decompLen)
              decompLen1 <-
                  c_decompressSafeContinue
                      ctx
                      compData
                      dstBegin
                      (coerce srcLen)
                      (coerce decompLen)
              assertM (decompLen == coerce decompLen1)
              let dstEnd = dstBegin `plusPtr` fromIntegral decompLen1
                  decompArr = MArray.Array fptr dstEnd dstMax
              Array.unsafeFreeze <$> MArray.shrinkToFit decompArr

--------------------------------------------------------------------------------
-- Compression
--------------------------------------------------------------------------------

{-# ANN type CompressState Fuse #-}
data CompressState st ctx prev
    = CompressInit st
    | CompressDo st ctx prev
    | CompressDone ctx

-- 64KB blocks are optimal as the dictionary max size is 64KB. We can rechunk
-- the stream into 64KB blocks before compression.
--
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

    {-# INLINE [0] step #-}
    -- FIXME: {-# INLINE_LATE step #-}
    step _ (CompressInit st) =
        liftIO
            $ do
                lz4Ctx <- c_createStream
                -- Instead of using an external dictionary we could just hold
                -- the previous chunks. However, the dictionary is only 64KB,
                -- if the chunk size is bigger we would be holding a lot more
                -- data than required. Also, the perf advantage does not seem
                -- much.
                return $ Stream.Skip $ CompressDo st lz4Ctx Nothing
    step gst (CompressDo st lz4Ctx prev) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> do
                arr1 <- liftIO $ compressChunk i lz4Ctx arr
                -- XXX touch the "prev" array to keep it alive?
                return $ Stream.Yield arr1 (CompressDo st1 lz4Ctx (Just arr))
            Stream.Skip st1 ->
                return $ Stream.Skip $ CompressDo st1 lz4Ctx prev
            Stream.Stop -> return $ Stream.Skip $ CompressDone lz4Ctx
    step _ (CompressDone lz4Ctx) =
        liftIO $ c_freeStream lz4Ctx >> return Stream.Stop

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
data DecompressState st ctx prev
    = DecompressInit st
    | DecompressDo st ctx prev
    | DecompressDone ctx

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

    {-# INLINE [0] step #-}
    -- FIXME: {-# INLINE_LATE step #-}
    step _ (DecompressInit st) =
        liftIO
            $ do
                lz4Ctx <- c_createStreamDecode
                return $ Stream.Skip $ DecompressDo st lz4Ctx Nothing
    step _ (DecompressDone lz4Ctx) =
        liftIO $ c_freeStreamDecode lz4Ctx >> return Stream.Stop
    step gst (DecompressDo st lz4Ctx prev) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> do
                arr1 <- liftIO $ decompressChunk lz4Ctx arr
                return $ Stream.Yield arr1 (DecompressDo st1 lz4Ctx (Just arr))
            Stream.Skip st1 ->
                return $ Stream.Skip $ DecompressDo st1 lz4Ctx prev
            Stream.Stop ->
                return $ Stream.Skip $ DecompressDone lz4Ctx
