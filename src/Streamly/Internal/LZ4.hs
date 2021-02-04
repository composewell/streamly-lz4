-- |
-- Module      : Streamly.Internal.LZ4
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Internal module subject to change without notice.
--
module Streamly.Internal.LZ4
    (
    -- * Foreign
      c_createStream
    , c_freeStream
    , c_createStreamDecode
    , c_freeStreamDecode

    -- * Primitives
    , compressChunk
    , decompressChunk

    -- * Streaming
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

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C (CInt(..), CString)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import Fusion.Plugin.Types (Fuse (..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array.Storable.Foreign as Array
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as Array
import qualified
    Streamly.Internal.Data.Array.Storable.Foreign.Mut.Types as MArray
import qualified Streamly.Internal.Data.Stream.StreamD as Stream

import Streamly.Internal.LZ4.Config

--------------------------------------------------------------------------------
-- CPP helpers
--------------------------------------------------------------------------------

-- Simple helpers for more informative inline statements.

#define INLINE_EARLY  INLINE [2]
#define INLINE_NORMAL INLINE [1]
#define INLINE_LATE   INLINE [0]

--------------------------------------------------------------------------------
-- Foreign
--------------------------------------------------------------------------------

data C_LZ4Stream

data C_LZ4StreamDecode

-- | Exported for unit tests
foreign import ccall unsafe "lz4.h LZ4_createStream"
    c_createStream :: IO (Ptr C_LZ4Stream)

-- | Exported for unit tests
foreign import ccall unsafe "lz4.h LZ4_freeStream"
    c_freeStream :: Ptr C_LZ4Stream -> IO ()

-- | Exported for unit tests
foreign import ccall unsafe "lz4.h LZ4_createStreamDecode"
    c_createStreamDecode :: IO (Ptr C_LZ4StreamDecode)

-- | Exported for unit tests
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

foreign import capi
    "lz4.h value LZ4_MAX_INPUT_SIZE" lz4_MAX_INPUT_SIZE :: CInt

lz4_MAX_OUTPUT_SIZE :: CInt
lz4_MAX_OUTPUT_SIZE =
    min (unsafePerformIO $ c_compressBound lz4_MAX_INPUT_SIZE) maxBound

--------------------------------------------------------------------------------
-- Conversion helpers
--------------------------------------------------------------------------------

{-# INLINE cIntToInt #-}
cIntToInt :: CInt -> Int
cIntToInt = fromIntegral

{-# INLINE unsafeIntToCInt #-}
unsafeIntToCInt :: Int -> CInt
unsafeIntToCInt = fromIntegral

{-# INLINE i32ToInt #-}
i32ToInt :: Int32 -> Int
i32ToInt = fromIntegral

{-# INLINE cIntToI32 #-}
cIntToI32 :: CInt -> Int32
cIntToI32 = coerce

{-# INLINE i32ToCInt #-}
i32ToCInt :: Int32 -> CInt
i32ToCInt = coerce

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
       Config a
    -> Int
    -> Ptr C_LZ4Stream
    -> Array.Array Word8
    -> IO (Array.Array Word8)
compressChunk Config{..} speed ctx arr = do
    Array.asPtr arr
        $ \src -> do
              let uncompLen = Array.byteLength arr
                  maxUncompLen = cIntToInt lz4_MAX_INPUT_SIZE
                  speedC = unsafeIntToCInt speed
              -- Ideally the maxCLen check below covers this case, but just in
              -- case.
              when (uncompLen > maxUncompLen)
                $ error $ "compressChunk: Source array length "
                    ++ show uncompLen
                    ++ " exceeds the max LZ4 limit "
                    ++ show maxUncompLen
              -- The size is safe to downcast
              let uncompLenC = unsafeIntToCInt uncompLen
              maxCompLenC <- c_compressBound uncompLenC
              let maxCompLen = cIntToInt maxCompLenC
              when (maxCompLenC <= 0)
                $ error $ "compressChunk: compressed length <= 0."
                    ++ " maxCompLenC: " ++ show maxCompLenC
                    ++ " uncompLenC: " ++ show uncompLenC
              -- allocate compressed block with 8 byte header.  First 4
              -- bytes of the header store the length of the uncompressed
              -- data and the next 4 bytes store the length of the
              -- compressed data.
              (MArray.Array fptr dstBegin dstMax) <-
                  MArray.newArray (maxCompLen + metaSize)
              let hdrCompLen = dstBegin `plusPtr` compSizeOffset
                  compData = dstBegin `plusPtr` dataOffset
              compLenC <-
                  c_compressFastContinue
                      ctx src compData uncompLenC maxCompLenC speedC
              when (compLenC <= 0)
                $ error $ "compressChunk: c_compressFastContinue failed. "
                    ++ "uncompLenC: " ++ show uncompLenC
                    ++ "compLenC: " ++ show compLenC
              setUncompSize dstBegin (cIntToI32 uncompLenC)
              poke hdrCompLen (cIntToI32 compLenC)
              let compLen = cIntToInt compLenC
                  dstEnd = dstBegin `plusPtr` (compLen + metaSize)
                  compArr = MArray.Array fptr dstEnd dstMax
              -- It is safe to shrink here as we need to hold the last 64KB of
              -- the previous uncompressed array and not the compressed one.
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
       Config a
    -> Ptr C_LZ4StreamDecode
    -> Array.Array Word8
    -> IO (Array.Array Word8)
decompressChunk Config{..} ctx arr = do
    Array.asPtr arr
        $ \src -> do
              let hdrCompLen :: Ptr Int32 = src `plusPtr` compSizeOffset
                  compData = src `plusPtr` dataOffset
                  arrDataLen = Array.byteLength arr - metaSize
              uncompLenC <- i32ToCInt <$> getUncompSize src
              compLenC <- i32ToCInt <$> peek hdrCompLen
              let compLen = cIntToInt compLenC
                  maxCompLenC = lz4_MAX_OUTPUT_SIZE
                  uncompLen = cIntToInt uncompLenC

              -- Error checks
              if compLenC <= 0
              then error "decompressChunk: compressed data length > 2GB"
              else if compLen < arrDataLen
              then error $ "decompressChunk: input array data length "
                ++ show arrDataLen ++ " is less than "
                ++ "the compressed data length specified in the header "
                ++ show compLen
              else when (compLenC > maxCompLenC) $
                  error $ "decompressChunk: compressed data length is more "
                    ++ "than the max limit: " ++ show maxCompLenC

              (MArray.Array fptr dstBegin dstMax) <- MArray.newArray uncompLen
              decompLenC <-
                  c_decompressSafeContinue
                        ctx compData dstBegin compLenC uncompLenC
              when (decompLenC < 0)
                $ error $ "decompressChunk: c_decompressSafeContinue failed. "
                    ++ "arrDataLen = " ++ show arrDataLen
                    ++ "compLenC = " ++ show compLenC
                    ++ "uncompLenC = " ++ show uncompLenC
                    ++ "decompLenC = " ++ show decompLenC
              let decompLen = cIntToInt decompLenC
                  dstEnd = dstBegin `plusPtr` decompLen
                  decompArr = MArray.Array fptr dstEnd dstMax
              -- We cannot shrink the array here, because that would reallocate
              -- the array invalidating the cached dictionary.
              return $ Array.unsafeFreeze decompArr

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
-- | See 'Streamly.LZ4.compress' for documentation.
{-# INLINE_NORMAL compressD #-}
compressD ::
       MonadIO m
    => Config a
    -> Int
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
compressD conf speed0 (Stream.Stream step0 state0) =
    Stream.Stream step (CompressInit state0)

    where

    speed = max speed0 0

    {-# INLINE_LATE step #-}
    step _ (CompressInit st) =
        liftIO
            $ do
                ctx <- c_createStream
                -- Instead of using an external dictionary we could just hold
                -- the previous chunks. However, the dictionary is only 64KB,
                -- if the chunk size is bigger we would be holding a lot more
                -- data than required. Also, the perf advantage does not seem
                -- much.
                return $ Stream.Skip $ CompressDo st ctx Nothing
    step gst (CompressDo st ctx prev) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 ->
                -- The compression primitives use 32-bit signed int (CInt) to
                -- represent the length of the array. The maximum value of a
                -- 32-bit signed int is 2GB.
                if Array.byteLength arr >= 2 * 1024 * 1024 * 1024
                then error "compressD: Array element > 2 GB encountered"
                else do
                    arr1 <- liftIO $ compressChunk conf speed ctx arr
                    -- XXX touch the "prev" array to keep it alive?
                    return $ Stream.Yield arr1 (CompressDo st1 ctx (Just arr))
            Stream.Skip st1 ->
                return $ Stream.Skip $ CompressDo st1 ctx prev
            Stream.Stop -> return $ Stream.Skip $ CompressDone ctx
    step _ (CompressDone ctx) =
        liftIO $ c_freeStream ctx >> return Stream.Stop

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

-- | This combinators resizes arrays to the required length. Every element of
-- the resulting stream will be a proper compressed element with 8 bytes of meta
-- data prefixed to it.
--
-- This has the property of idempotence,
-- @resizeD . resizeD = resizeD@
--
{-# INLINE_NORMAL resizeD #-}
resizeD ::
       MonadIO m
    => Config a
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
resizeD Config{..} (Stream.Stream step0 state0) =
    Stream.Stream step (RInit state0)

    where

    {-# INLINE process #-}
    process st arr@(Array.Array fb e) = do
        let len = Array.byteLength arr
        if len <= metaSize
        then return $ Stream.Skip $ RAccumlate st arr
        else withForeignPtr fb
                 $ \b -> do
                       let compLenPtr = castPtr (b `plusPtr` compSizeOffset)
                       compressedSize <- i32ToInt <$> peek compLenPtr
                       let required = compressedSize + metaSize
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

    {-# INLINE_LATE step #-}
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

-- | This combinator assumes all the arrays in the incoming stream are properly
-- resized.
--
-- This combinator works well with untouched arrays compressed with 'compressD'.
-- A random compressed stream would first need to be resized properly with
-- 'resizeD'.
--
{-# INLINE_NORMAL decompressResizedD #-}
decompressResizedD ::
       MonadIO m
    => Config a
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
decompressResizedD conf (Stream.Stream step0 state0) =
    Stream.Stream step (DecompressInit state0)

   where

    {-# INLINE_LATE step #-}
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
                arr1 <- liftIO $ decompressChunk conf lz4Ctx arr
                -- Instead of the input array chunk we need to hold the output
                -- array chunk here.
                return $ Stream.Yield arr1 (DecompressDo st1 lz4Ctx (Just arr1))
            Stream.Skip st1 ->
                return $ Stream.Skip $ DecompressDo st1 lz4Ctx prev
            Stream.Stop -> return $ Stream.Skip $ DecompressDone lz4Ctx
