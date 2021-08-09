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
    , compressChunksD
    , compressChunksDFrame
    , resizeChunksD
    , decompressChunksRawD

    -- * Parsing
    , simpleFrameParserD
    , decompressChunksWithD
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
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (second)
import Data.Bits (Bits(..))
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C (CInt(..), CString)
import Foreign.ForeignPtr (plusForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import Fusion.Plugin.Types (Fuse (..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MArray
import qualified Streamly.Internal.Data.Parser.ParserD as Parser
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.StreamD as Stream
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as ArrayFold

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
       BlockFormat
    -> Int
    -> Ptr C_LZ4Stream
    -> Array.Array Word8
    -> IO (Array.Array Word8)
compressChunk cfg speed ctx arr = do
    Array.unsafeAsPtr arr
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
                  MArray.newArray (maxCompLen + metaSize_)
              let hdrCompLen = dstBegin `plusPtr` compSizeOffset_
                  compData = dstBegin `plusPtr` dataOffset_
              compLenC <-
                  c_compressFastContinue
                      ctx src compData uncompLenC maxCompLenC speedC
              when (compLenC <= 0)
                $ error $ "compressChunk: c_compressFastContinue failed. "
                    ++ "uncompLenC: " ++ show uncompLenC
                    ++ "compLenC: " ++ show compLenC
              setUncompSize_ dstBegin (cIntToI32 uncompLenC)
              poke hdrCompLen (cIntToI32 compLenC)
              let compLen = cIntToInt compLenC
                  dstEnd = dstBegin `plusPtr` (compLen + metaSize_)
                  compArr = MArray.Array fptr dstEnd dstMax
              -- It is safe to shrink here as we need to hold the last 64KB of
              -- the previous uncompressed array and not the compressed one.
              Array.unsafeFreeze <$> MArray.shrinkToFit compArr
    where

    metaSize_ = metaSize cfg
    compSizeOffset_ = compSizeOffset cfg
    dataOffset_ = dataOffset cfg
    setUncompSize_ = setUncompSize cfg

-- Having NOINLINE here does not effect the performance a lot. Every
-- iteration of the loop is a little slower (< 1us) but the entire loop
-- fuses.
--
-- With INLINE statement and the usage of fusion-plugin results in an
-- enormous code size when used with other combinators.
-- | Primitive function to decompress a chunk of Word8.
{-# NOINLINE decompressChunk #-}
decompressChunk ::
       BlockFormat
    -> Ptr C_LZ4StreamDecode
    -> Array.Array Word8
    -> IO (Array.Array Word8)
decompressChunk cfg ctx arr = do
    Array.unsafeAsPtr arr
        $ \src -> do
              let hdrCompLen :: Ptr Int32 = src `plusPtr` compSizeOffset cfg
                  compData = src `plusPtr` dataOffset cfg
                  arrDataLen = Array.byteLength arr - metaSize cfg
              uncompLenC <- i32ToCInt <$> getUncompSize cfg src
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
                    ++ "\narrDataLen = " ++ show arrDataLen
                    ++ "\ncompLenC = " ++ show compLenC
                    ++ "\nuncompLenC = " ++ show uncompLenC
                    ++ "\ndecompLenC = " ++ show decompLenC
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
{-# INLINE_NORMAL compressChunksD #-}
compressChunksD ::
       MonadIO m
    => BlockFormat
    -> Int
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
compressChunksD cfg speed0 (Stream.Stream step0 state0) =
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
                then error "compressChunksD: Array element > 2 GB encountered"
                else do
                    arr1 <- liftIO $ compressChunk cfg speed ctx arr
                    -- XXX touch the "prev" array to keep it alive?
                    return $ Stream.Yield arr1 (CompressDo st1 ctx (Just arr))
            Stream.Skip st1 ->
                return $ Stream.Skip $ CompressDo st1 ctx prev
            Stream.Stop -> return $ Stream.Skip $ CompressDone ctx
    step _ (CompressDone ctx) =
        liftIO $ c_freeStream ctx >> return Stream.Stop

-- XXX This wont fuse and should be improved.
-- XXX Only here for testing and benchmarking
-- | See 'Streamly.LZ4.compress' for documentation.
{-# INLINE_NORMAL compressChunksDFrame #-}
compressChunksDFrame ::
       MonadIO m
    => BlockFormat
    -> FrameFormat
    -> Int
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
compressChunksDFrame bf FrameFormat {hasEndMark} speed strm =
    if hasEndMark
    then compressChunksD bf speed strm
             `Stream.append` Stream.fromPure endMarkArr
    else compressChunksD bf speed strm

--------------------------------------------------------------------------------
-- Decompression
--------------------------------------------------------------------------------

{-# ANN type ResizeState Fuse #-}
data ResizeState st arr
    = RInit st
    | RProcess st arr
    | RAccumulate st arr
    | RFooter st arr
    | RYield arr (ResizeState st arr)
    | RDone

-- | This combinators resizes arrays to the required length. Every element of
-- the resulting stream will be a proper compressed element with 8 bytes of meta
-- data prefixed to it.
--
-- This has the property of idempotence,
-- @resizeChunksD . resizeChunksD = resizeChunksD@
--
{-# INLINE_NORMAL resizeChunksD #-}
resizeChunksD ::
       MonadIO m
    => BlockFormat
    -> FrameFormat
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
resizeChunksD cfg conf (Stream.Stream step0 state0) =
    Stream.Stream step (RInit state0)

    where

    metaSize_ = metaSize cfg
    compSizeOffset_ = compSizeOffset cfg

    hasEndMark_ = hasEndMark conf
    footerSize_ = footerSize conf
    validateFooter_ = validateFooter conf

    -- Unsafe function
    {-# INLINE isEndMark #-}
    isEndMark src
        | hasEndMark_ = do
              em <- peek (castPtr src :: Ptr Int32)
              return $ em == endMark
        | otherwise = return False

    {-# INLINE process #-}
    process st arr@(Array.Array fb e) = do
        let len = Array.byteLength arr
        if len < 4
        then return $ Stream.Skip $ RAccumulate st arr
        else withForeignPtr fb $ \b -> do
               res <- isEndMark b
               if res
               then return $ Stream.Skip $ RFooter st arr
               else do
                   if len <= metaSize_
                   then return $ Stream.Skip $ RAccumulate st arr
                   else do
                       let compLenPtr = castPtr (b `plusPtr` compSizeOffset_)
                       compressedSize <- i32ToInt <$> peek compLenPtr
                       let required = compressedSize + metaSize_
                       if len == required
                       then return $ Stream.Skip $ RYield arr $ RInit st
                       else if len < required
                       then return $ Stream.Skip $ RAccumulate st arr
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
            Stream.Stop ->
                if hasEndMark_
                then error "resizeChunksD: No end mark found"
                else return Stream.Stop
    step _ (RProcess st arr) = liftIO $ process st arr
    step gst (RAccumulate st buf) = do
        r <- step0 gst st
        case r of
            Stream.Yield arr st1 -> do
                arr1 <- Array.spliceTwo buf arr
                liftIO $ process st1 arr1
            Stream.Skip st1 -> return $ Stream.Skip $ RAccumulate st1 buf
            Stream.Stop -> error "resizeChunksD: Incomplete block"
    step gst (RFooter st buf) = do
        -- Warn if len > footerSize
        let len = Array.byteLength buf
        if len < footerSize_
        then do
            r <- step0 gst st
            case r of
                Stream.Yield arr st1 -> do
                    arr1 <- Array.spliceTwo buf arr
                    return $ Stream.Skip $ RFooter st1 arr1
                Stream.Skip st1 -> return $ Stream.Skip $ RFooter st1 buf
                Stream.Stop -> error "resizeChunksD: Incomplete footer"
        else do
            res <- liftIO $ validateFooter_ buf
            if res
            then return Stream.Stop
            else error "resizeChunksD: Invalid footer"
    step _ RDone = return Stream.Stop

{-# ANN type DecompressState Fuse #-}
data DecompressState st ctx prev
    = DecompressInit st
    | DecompressDo st ctx prev
    | DecompressDone ctx

-- | This combinator assumes all the arrays in the incoming stream are properly
-- resized.
--
-- This combinator works well with untouched arrays compressed with 'compressChunksD'.
-- A random compressed stream would first need to be resized properly with
-- 'resizeChunksD'.
--
{-# INLINE_NORMAL decompressChunksRawD #-}
decompressChunksRawD ::
       MonadIO m
    => BlockFormat
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
decompressChunksRawD cfg (Stream.Stream step0 state0) =
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
                arr1 <- liftIO $ decompressChunk cfg lz4Ctx arr
                -- Instead of the input array chunk we need to hold the output
                -- array chunk here.
                return $ Stream.Yield arr1 (DecompressDo st1 lz4Ctx (Just arr1))
            Stream.Skip st1 ->
                return $ Stream.Skip $ DecompressDo st1 lz4Ctx prev
            Stream.Stop -> return $ Stream.Skip $ DecompressDone lz4Ctx

decompressChunksWithD ::
       (MonadThrow m, MonadIO m)
    => Parser.Parser m Word8 (BlockFormat, FrameFormat)
    -> Stream.Stream m (Array.Array Word8)
    -> Stream.Stream m (Array.Array Word8)
decompressChunksWithD p s = do
    ((cfg, config), next) <- Stream.fromEffect $ second Stream.toStreamD
        <$> ArrayStream.fold_ (ArrayFold.fromParser p) (Stream.fromStreamD s)
    decompressChunksRawD cfg (resizeChunksD cfg config next)

-- XXX Merge this with BlockFormat?
data FLG =
    FLG
        { isBlockIndependent :: Bool
        , hasBlockChecksum :: Bool
        , hasContentSize :: Bool
        , hasContentChecksum :: Bool
        , hasDict :: Bool
        }

simpleFrameParserD ::
       (Monad m, MonadThrow m)
    => Parser.Parser m Word8 (BlockFormat, FrameFormat)
simpleFrameParserD = do
    _ <- assertMagic
    _flg <- parseFLG
    blockMaxSize <- parseBD
    _ <- assertHeaderChecksum
    let config =
            (BlockFormat {blockSize = blockMaxSize}
            , FrameFormat {hasEndMark = True})
    Parser.fromPure config

    where

    assertHeaderChecksum = Parser.satisfy (const True)

    assertMagic = do
        let magic = 407708164 :: Int
        magic_ <-
            let w8ToInt = fromIntegral :: Word8 -> Int
                stp (i, b) a = (i + 8, b + w8ToInt a * 2 ^ i) :: (Int, Int)
                fld = Fold.foldl' stp (0, 0)
             in Parser.takeEQ 4 (snd <$> fld)
        if magic_ == magic
        then Parser.fromPure ()
        else Parser.die
                 ("The parsed magic "
                     ++ show magic_ ++ " does not match " ++ show magic)

    parseFLG = do
        a <- Parser.satisfy (const True)
        let isVersion01 = not (testBit a 7) && testBit a 6
        let flg =
                FLG
                    (testBit a 5)
                    (testBit a 4)
                    (testBit a 3)
                    (testBit a 2)
                    (testBit a 0)
        if isVersion01
        then if isBlockIndependent flg
        then Parser.die "Block independence is not yet supported"
        else if hasBlockChecksum flg
        then Parser.die "Block checksum is not yet supported"
        else if hasContentSize flg
        then Parser.die "Content size is not yet supported"
        else if hasContentChecksum flg
        then Parser.die "Content checksum is not yet supported"
        else if hasDict flg
        then Parser.die "Dict is not yet supported"
        else Parser.fromPure flg
        else Parser.die "Version is not 01"

    parseBD = do
        a <- Parser.satisfy (const True)
        case shiftR a 4 of
            4 -> Parser.fromPure BlockMax64KB
            5 -> Parser.fromPure BlockMax256KB
            6 -> Parser.fromPure BlockMax1MB
            7 -> Parser.fromPure BlockMax4MB
            _ -> Parser.die "parseBD: Unknown block max size"
