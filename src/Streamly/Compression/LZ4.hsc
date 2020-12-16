module Streamly.Compression.LZ4
    ( compressD
    , resizeD
    , resize
    , decompressResizedD
    , decompressD
    , compress
    , decompressResized
    , decompress
    , debugD
    , debug
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word32, Word8)
import Foreign.C (CInt(..), CString)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Marshal (copyBytes, free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Mut.Types as MA
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Streamly.Prelude

--------------------------------------------------------------------------------
-- CPP helpers
--------------------------------------------------------------------------------

-- Simple helpers for more informative inline statements.

-- XXX FIXME
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
-- FIXME: {-# INLINE_NORMAL debugD #-}
debugD :: MonadIO m => D.Stream m (A.Array Word8) -> D.Stream m (A.Array Word8)
debugD (D.Stream step0 state0) = D.Stream step (0 :: Int, state0)

    where

    {-# INLINE putDivider #-}
    putDivider = putStrLn "---------------------------------"

    {-# INLINE debugger #-}
    debugger i arr@(A.Array fb _) = do
        let len = A.byteLength arr
        if len <= 8
        then do
            putDivider
            putStrLn $ "Index  : " ++ show i
            putStrLn $ "Length : " ++ show len
            putStrLn $ "Size info isn't available"
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

    -- FIXME: {-# INLINE_LATE step #-}
    step gst (i, st) = do
        r <- step0 gst st
        case r of
            D.Yield arr st1 -> do
                liftIO $ debugger i arr
                return $ D.Yield arr (i + 1, st1)
            D.Skip st1 -> return $ D.Skip $ (i, st1)
            D.Stop -> do
                liftIO putDivider
                return $ D.Stop

-- | A simple combinator that prints the index, length, compressed size and
-- decompressed size of each array element to standard output.
--
-- This only works on stream of resized arrays.
--
debug :: MonadIO m => SerialT m (A.Array Word8) -> SerialT m (A.Array Word8)
debug m = D.fromStreamD (debugD (D.toStreamD m))

--------------------------------------------------------------------------------
-- Compression
--------------------------------------------------------------------------------

data CompressState st strm dict
    = CInit st
    | CProcess st strm dict
    | CCleanup strm dict

-- | See 'compress' for documentation.
-- FIXME: {-# INLINE_NORMAL compressD #-}
compressD ::
       MonadIO m
    => Int
    -> D.Stream m (A.Array Word8)
    -> D.Stream m (A.Array Word8)
compressD i0 (D.Stream step0 state0) = D.Stream step (CInit state0)

    where

    i = fromIntegral $ max i0 0

    {-# INLINE compressChunk #-}
    compressChunk arr@(A.Array fb _) strm dict = do
        withForeignPtr fb
            $ \b -> do
                  let cstr = castPtr b
                      clen = fromIntegral $ A.byteLength arr
                  olen <- c_compressBound clen
                  (MA.Array fbe _ en) <- MA.newArray (fromIntegral olen + 8)
                  withForeignPtr fbe
                      $ \be -> do
                            size <-
                                c_compressFastContinue
                                    strm cstr (be `plusPtr` 8) clen olen i
                            void $ c_saveDict strm dict (64 * 1024)
                            let cbe = castPtr be
                            poke cbe (fromIntegral clen :: Word32)
                            poke (cbe `plusPtr` 4) (fromIntegral size :: Word32)
                            let bo1 = be `plusPtr` (fromIntegral size + 8)
                            arr1 <-
                                A.unsafeFreeze
                                    <$> MA.shrinkToFit (MA.Array fbe bo1 en)
                            return arr1

    -- FIXME: {-# INLINE_LATE step #-}
    step _ (CInit st) =
        liftIO
            $ do
                strm <- c_createStream
                dict <- mallocBytes (64 * 1024) :: IO CString
                return $ D.Skip $ CProcess st strm dict
    step _ (CCleanup strm dict) =
        liftIO $ c_freeStream strm >> free dict >> return D.Stop
    step gst (CProcess st strm dict) = do
        r <- step0 gst st
        case r of
            D.Yield arr st1 -> do
                arr1 <- liftIO $ compressChunk arr strm dict
                return $ D.Yield arr1 (CProcess st1 strm dict)
            D.Skip st1 -> return $ D.Skip $ CProcess st1 strm dict
            D.Stop -> return $ D.Skip $ CCleanup strm dict

-- | Compress a stream of foreign arrays (< 2GB / element) using LZ4 stream
-- compression.
--
-- @compress i m@ compresses the stream @m@ using the acceleration value @i@.
--
-- As the acceleration increases, the compression speed increases whereas the
-- compression ratio decreases.
--
compress ::
       MonadIO m
    => Int
    -> SerialT m (A.Array Word8)
    -> SerialT m (A.Array Word8)
compress i m = D.fromStreamD (compressD i (D.toStreamD m))

--------------------------------------------------------------------------------
-- Decompression
--------------------------------------------------------------------------------

data ResizeState st arr
    = RInit st
    | RProcess st arr
    | RAccumlate st arr
    | RDone

-- | See 'resize' for documentation.
--
resizeD :: MonadIO m => D.Stream m (A.Array Word8) -> D.Stream m (A.Array Word8)
resizeD (D.Stream step0 state0) = D.Stream step (RInit state0)

    where

    {-# INLINE process #-}
    process st arr@(A.Array fb e) = do
        let len = A.byteLength arr
        if len <= 8
        then return $ D.Skip $ RAccumlate st arr
        else withForeignPtr fb
                 $ \b -> do
                       compressedSize <-
                           peek (castPtr (b `plusPtr` 4) :: Ptr Word32)
                       let required = fromIntegral compressedSize + 8
                       if len == required
                       then return $ D.Yield arr $ RInit st
                       else if len < required
                       then return $ D.Skip $ RAccumlate st arr
                       else do
                           let arr1E = b `plusPtr` required
                               arr1 = A.Array fb arr1E
                               arr2S = fb `plusForeignPtr` required
                               arr2 = A.Array arr2S e
                           return $ D.Yield arr1 $ RProcess st arr2

    step gst (RInit st) = do
        r <- step0 gst st
        case r of
            D.Yield arr st1 -> liftIO $ process st1 arr
            D.Skip st1 -> return $ D.Skip $ RInit st1
            D.Stop -> return D.Stop
    step _ (RProcess st arr) = liftIO $ process st arr
    step gst (RAccumlate st buf) = do
        r <- step0 gst st
        case r of
            D.Yield arr st1 -> do
                arr1 <- A.spliceTwo buf arr
                liftIO $ process st1 arr1
            D.Skip st1 -> return $ D.Skip $ RAccumlate st1 buf
            D.Stop -> return $ D.Yield buf $ RDone
    step _ RDone = return D.Stop

-- | This combinators resizes arrays to the required length. Every element of
-- the resulting stream will be a proper compressed element with 8 bytes of meta
-- data prefixed to it.
--
resize :: MonadIO m => SerialT m (A.Array Word8) -> SerialT m (A.Array Word8)
resize m = D.fromStreamD (resizeD (D.toStreamD m))

data DecompressState buf st dict dsize
    = DInit st
    | DProcess st dict dsize
    | DCleanup dict

-- | See 'decompressResized' for documentation.
--
decompressResizedD ::
       MonadIO m => D.Stream m (A.Array Word8) -> D.Stream m (A.Array Word8)
decompressResizedD (D.Stream step0 state0) = D.Stream step (DInit state0)

   where

    {-# INLINE decompressChunk #-}
    decompressChunk (A.Array fb _) dict dsize = do
        withForeignPtr fb
            $ \b -> do
                  decompressedSize <- peek (castPtr b :: Ptr Word32)
                  compressedSize <-
                      peek (castPtr (b `plusPtr` 4) :: Ptr Word32)
                  let cstr = castPtr (b `plusPtr` 8)
                  (MA.Array fbe _ en) <-
                      MA.newArray (fromIntegral decompressedSize)
                  withForeignPtr fbe
                      $ \be -> do
                            dsize1 <-
                                c_decompressSafeUsingDict
                                    cstr be (fromIntegral compressedSize)
                                        (fromIntegral decompressedSize)
                                            dict dsize
                            copyBytes
                                dict be (min (fromIntegral dsize1) (64 * 1024))
                            let bo1 = be `plusPtr` (fromIntegral dsize1)
                            arr1 <-
                                A.unsafeFreeze
                                    <$> MA.shrinkToFit (MA.Array fbe bo1 en)
                            return (arr1, dsize1)

    -- FIXME: {-# INLINE_LATE step #-}
    step _ (DInit st) =
        liftIO
            $ do
                dict <- mallocBytes (64 * 1024) :: IO (Ptr Word8)
                return $ D.Skip $ DProcess st dict 0
    step _ (DCleanup dict) =
        liftIO $ free dict >> return D.Stop
    step gst (DProcess st dict dsize) = do
        r <- step0 gst st
        case r of
            D.Yield arr st1 -> do
                (arr1, dsize1) <- liftIO $ decompressChunk arr dict dsize
                return $ D.Yield arr1 (DProcess st1 dict dsize1)
            D.Skip st1 -> return $ D.Skip $ DProcess st1 dict dsize
            D.Stop -> return $ D.Skip $ DCleanup dict

-- | See 'decompress' for documentation.
--
decompressD ::
       MonadIO m => D.Stream m (A.Array Word8) -> D.Stream m (A.Array Word8)
decompressD = decompressResizedD . resizeD

-- | This combinator assumes all the arrays in the incoming stream are properly
-- resized.
--
-- This combinator works well with untouched arrays compressed with 'compressD'
-- but a random compressed stream should first be resized properly  with
-- 'resizeD'.
--
decompressResized ::
       MonadIO m => SerialT m (A.Array Word8) -> SerialT m (A.Array Word8)
decompressResized m = D.fromStreamD (decompressResizedD (D.toStreamD m))

-- | Decompress a stream of arrays compressed using LZ4 stream compression.
decompress ::
       MonadIO m => SerialT m (A.Array Word8) -> SerialT m (A.Array Word8)
decompress m = D.fromStreamD (decompressD (D.toStreamD m))
