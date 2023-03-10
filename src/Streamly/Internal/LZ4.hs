{-# LANGUAGE NamedFieldPuns, TypeApplications #-}
-- |
-- Module      : Streamly.Internal.LZ4
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- LZ4 compression and decompression routines.
--
module Streamly.Internal.LZ4
    (
    -- * Foreign
      c_createStream
    , c_freeStream
    , c_createStreamDecode
    , c_freeStreamDecode

    -- * Block compression and decompression
    , compressChunk
    , decompressChunk

    -- * Stream compression and decompression
    , compressChunksD
    , resizeChunksD
    , decompressChunksRawD
    , decompressChunksWithD

    -- * Parsing LZ4 Frames
    , simpleFrameParserD
    )

where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (Bits(..))
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Word (Word32, Word8, byteSwap32)
import Foreign.C (CInt(..), CString)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import Fusion.Plugin.Types (Fuse (..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.MutArray as MArray
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Unboxed as Unbox

import qualified Streamly.Internal.Data.Fold.Chunked as ArrayFold
    ( fromParserD )
import qualified Streamly.Internal.Data.Stream.Chunked as ArrayStream
    ( runArrayFoldBreak )
import qualified Streamly.Internal.Data.Array as Array
    ( asPtrUnsafe, castUnsafe, unsafeFreeze, unsafeThaw )
import qualified Streamly.Internal.Data.Array.Mut.Type as MArray
    ( MutArray(..), asPtrUnsafe, rightSize, touch )
import qualified Streamly.Internal.Data.Array.Type as Array
    ( Array(..), byteLength, splice )
import qualified Streamly.Internal.Data.Parser.ParserD as Parser
    ( Parser, ParseError(..), die, fromPure, satisfy, takeEQ )
import qualified Streamly.Internal.Data.Stream as Stream
    ( concatMapM, fromPure, fromStreamK, toStreamK )
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD
    ( Step(..), Stream(..))

import Streamly.Internal.LZ4.Config

--------------------------------------------------------------------------------
-- CPP helpers
--------------------------------------------------------------------------------

-- Simple helpers for more informative inline statements.

#define INLINE_EARLY  INLINE [2]
#define INLINE_NORMAL INLINE [1]
#define INLINE_LATE   INLINE [0]

--------------------------------------------------------------------------------
-- Endianess
--------------------------------------------------------------------------------

{-# NOINLINE isLittleEndianMachine #-}
isLittleEndianMachine :: Bool
isLittleEndianMachine =
    let lsb = head $ Array.toList $ Array.asBytes $ Array.fromList [1 :: Word32]
     in lsb == 1

{-# INLINE toLittleEndian #-}
toLittleEndian :: Int32 -> Int32
toLittleEndian i32
    | isLittleEndianMachine = i32
    | otherwise = fromIntegral (byteSwap32 (fromIntegral i32))

{-# INLINE fromLittleEndian #-}
fromLittleEndian :: Int32 -> Int32
fromLittleEndian = toLittleEndian

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

-------------------------------------------------------------------------------
-- Block Configuration access
-------------------------------------------------------------------------------

metaSize :: BlockConfig -> Int
metaSize BlockConfig {blockSize} =
    case blockSize of
        BlockHasSize -> 8
        _ -> 4

setUncompSize :: BlockConfig -> Ptr Word8 -> Int32 -> IO ()
setUncompSize BlockConfig {blockSize} =
    case blockSize of
        BlockHasSize -> \src -> poke (castPtr src `plusPtr` 4) . toLittleEndian
        _ -> \_ _ -> return ()

getUncompSize :: BlockConfig -> Ptr Word8 -> IO Int32
getUncompSize BlockConfig {blockSize} =
    case blockSize of
        BlockHasSize ->
            \src ->
                fromLittleEndian <$> peek (castPtr src `plusPtr` 4 :: Ptr Int32)
        BlockMax64KB -> \_ -> return $ 64 * 1024
        BlockMax256KB -> \_ -> return $ 256 * 1024
        BlockMax1MB -> \_ -> return $ 1024 * 1024
        BlockMax4MB -> \_ -> return $ 4 * 1024 * 1024

dataOffset :: BlockConfig -> Int
dataOffset BlockConfig {blockSize} =
    case blockSize of
        BlockHasSize -> 8
        _ -> 4

compSizeOffset :: BlockConfig -> Int
compSizeOffset _ = 0

--------------------------------------------------------------------------------
-- Block level compression and decompression
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
--
-- | Compress an array of Word8. The compressed block header depends on the
-- 'BlockConfig' setting.
{-# NOINLINE compressChunk #-}
compressChunk ::
       BlockConfig
    -> Int
    -> Ptr C_LZ4Stream
    -> Array.Array Word8
    -> IO (Array.Array Word8)
compressChunk cfg speed ctx arr = do
    Array.asPtrUnsafe (Array.castUnsafe arr)
        $ \src -> do
              let uncompLen = Array.byteLength arr
                  speedC = unsafeIntToCInt speed
              when (uncompLen > maxBlockSize)
                $ error $ "compressChunk: Source array length "
                    ++ show uncompLen
                    ++ " exceeds the maximum block size of "
                    ++ show maxBlockSize
              -- The size is safe to downcast
              let uncompLenC = unsafeIntToCInt uncompLen
              maxCompLenC <- c_compressBound uncompLenC
              let maxCompLen = cIntToInt maxCompLenC
              when (maxCompLenC <= 0)
                $ error $ "compressChunk: compressed length <= 0."
                    ++ " maxCompLenC: " ++ show maxCompLenC
                    ++ " uncompLenC: " ++ show uncompLenC
              newarr@(MArray.MutArray cont arrStart arrEnd arrBound) <-
                  MArray.newPinned @IO @Word8 (maxCompLen + metaSize_)
              dstBegin_ <- MArray.asPtrUnsafe newarr return
              let dstBegin = dstBegin_ `plusPtr` arrEnd
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
              poke hdrCompLen (toLittleEndian (cIntToI32 compLenC))
              let compLen = cIntToInt compLenC
                  dstEnd = arrStart + compLen + metaSize_
                  compArr = MArray.MutArray cont arrStart dstEnd arrBound
              -- It is safe to shrink here as we need to hold the last 64KB of
              -- the previous uncompressed array and not the compressed one.
              Array.unsafeFreeze <$> MArray.rightSize compArr
    where

    metaSize_ = metaSize cfg
    compSizeOffset_ = compSizeOffset cfg
    dataOffset_ = dataOffset cfg
    setUncompSize_ = setUncompSize cfg
    maxBlockSize =
        case blockSize cfg of
             BlockHasSize -> cIntToInt lz4_MAX_INPUT_SIZE
             BlockMax64KB -> 64 * 1024
             BlockMax256KB -> 256 * 1024
             BlockMax1MB -> 1024 * 1024
             BlockMax4MB -> 4 * 1024 * 1024

-- Having NOINLINE here does not effect the performance a lot. Every
-- iteration of the loop is a little slower (< 1us) but the entire loop
-- fuses.
--
-- With INLINE statement and the usage of fusion-plugin results in an
-- enormous code size when used with other combinators.
-- | Primitive function to decompress a chunk of Word8.
{-# NOINLINE decompressChunk #-}
decompressChunk ::
       BlockConfig
    -> Ptr C_LZ4StreamDecode
    -> Array.Array Word8
    -> IO (Array.Array Word8)
decompressChunk cfg ctx arr = do
    Array.asPtrUnsafe (Array.castUnsafe arr)
        $ \src -> do
              let hdrCompLen :: Ptr Int32 = src `plusPtr` compSizeOffset cfg
                  compData = src `plusPtr` dataOffset cfg
                  arrDataLen = Array.byteLength arr - metaSize cfg
              uncompLenC <- i32ToCInt <$> getUncompSize cfg src
              compLenC <- i32ToCInt . fromLittleEndian <$> peek hdrCompLen
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

              newarr@(MArray.MutArray cont arrStart arrEnd arrBound)
                  <- MArray.newPinned @IO @Word8 uncompLen
              dstBegin_ <- MArray.asPtrUnsafe newarr return
              let dstBegin = dstBegin_ `plusPtr` arrEnd
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
                  dstEnd = arrStart + decompLen
                  decompArr = MArray.MutArray cont arrStart dstEnd arrBound
              -- We cannot shrink the array here, because that would reallocate
              -- the array invalidating the cached dictionary.
              return $ Array.unsafeFreeze decompArr

--------------------------------------------------------------------------------
-- Stream compression
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
    => BlockConfig
    -> Int
    -> StreamD.Stream m (Array.Array Word8)
    -> StreamD.Stream m (Array.Array Word8)
compressChunksD cfg speed0 (StreamD.Stream step0 state0) =
    StreamD.Stream step (CompressInit state0)

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
                return $ StreamD.Skip $ CompressDo st ctx Nothing
    step gst (CompressDo st ctx prev) = do
        r <- step0 gst st
        case r of
            StreamD.Yield arr st1 ->
                -- The compression primitives use 32-bit signed int (CInt) to
                -- represent the length of the array. The maximum value of a
                -- 32-bit signed int is 2GB.
                if Array.byteLength arr >= 2 * 1024 * 1024 * 1024
                then error "compressChunksD: Array element > 2 GB encountered"
                else do
                    arr1 <- liftIO $ compressChunk cfg speed ctx arr
                    -- XXX touch the "prev" array to keep it alive?
                    return $ StreamD.Yield arr1 (CompressDo st1 ctx (Just arr))
            StreamD.Skip st1 ->
                return $ StreamD.Skip $ CompressDo st1 ctx prev
            StreamD.Stop -> return $ StreamD.Skip $ CompressDone ctx
    step _ (CompressDone ctx) =
        liftIO $ c_freeStream ctx >> return StreamD.Stop

--------------------------------------------------------------------------------
-- Stream decompression
--------------------------------------------------------------------------------

{-# INLINE endMark #-}
endMark :: Int32
endMark = 0

footerSize :: FrameConfig -> Int
footerSize FrameConfig {hasEndMark} =
    if hasEndMark
    then 4
    else 0

validateFooter :: FrameConfig -> Array.Array Word8 -> IO Bool
validateFooter _ _ = return True

{-# ANN type ResizeState Fuse #-}
data ResizeState st arr
    = RInit st
    | RProcess st arr
    | RAccumulate st arr
    | RFooter st arr
    | RYield arr (ResizeState st arr)
    | RDone

-- | Look for a compressed block header and compact the arrays in the input
-- stream to the compressed length specified in the header. The output contains
-- arrays, where each array represents a full single compressed block along
-- with the compression header.
--
-- The resize operation is idempotent:
--
-- @resizeChunksD . resizeChunksD = resizeChunksD@
--
{-# INLINE_NORMAL resizeChunksD #-}
resizeChunksD ::
       MonadIO m
    => BlockConfig
    -> FrameConfig
    -> StreamD.Stream m (Array.Array Word8)
    -> StreamD.Stream m (Array.Array Word8)
resizeChunksD cfg conf (StreamD.Stream step0 state0) =
    StreamD.Stream step (RInit state0)

    where

    metaSize_ = metaSize cfg
    compSizeOffset_ = compSizeOffset cfg

    hasEndMark_ = hasEndMark conf
    footerSize_ = footerSize conf
    validateFooter_ = validateFooter conf

    -- Unsafe function
    {-# INLINE isEndMark #-}
    isEndMark arrContents i
        | hasEndMark_ = do
              em <- Unbox.peekWith arrContents i
              return $ em == endMark
        | otherwise = return False

    {-# INLINE process #-}
    process st arr@(Array.Array cont b e) = do
        let len = Array.byteLength arr
        if len < 4
        then return $ StreamD.Skip $ RAccumulate st arr
        else do
               res <- isEndMark (MArray.arrContents $ Array.unsafeThaw arr) b
               if res
               then return $ StreamD.Skip $ RFooter st arr
               else do
                   if len <= metaSize_
                   then return $ StreamD.Skip $ RAccumulate st arr
                   else do
                       let compLenPtr = b + compSizeOffset_
                       compressedSize <-
                           i32ToInt . fromLittleEndian <$>
                                    Unbox.peekWith
                                           (MArray.arrContents $ Array.unsafeThaw arr)
                                           compLenPtr
                       let required = compressedSize + metaSize_
                       if len == required
                       then return $ StreamD.Skip $ RYield arr $ RInit st
                       else if len < required
                       then return $ StreamD.Skip $ RAccumulate st arr
                       else do
                           let arr1E = b + required
                               arr1 = Array.Array cont b arr1E
                               arr2 = Array.Array cont arr1E e
                           MArray.touch cont
                           return $ StreamD.Skip $ RYield arr1 $ RProcess st arr2

    {-# INLINE_LATE step #-}
    step _ (RYield r next) = return $ StreamD.Yield r next
    step gst (RInit st) = do
        r <- step0 gst st
        case r of
            StreamD.Yield arr st1 -> liftIO $ process st1 arr
            StreamD.Skip st1 -> return $ StreamD.Skip $ RInit st1
            StreamD.Stop ->
                if hasEndMark_
                then error "resizeChunksD: No end mark found"
                else return StreamD.Stop
    step _ (RProcess st arr) = liftIO $ process st arr
    step gst (RAccumulate st buf) = do
        r <- step0 gst st
        case r of
            StreamD.Yield arr st1 -> do
                arr1 <- Array.splice buf arr
                liftIO $ process st1 arr1
            StreamD.Skip st1 -> return $ StreamD.Skip $ RAccumulate st1 buf
            StreamD.Stop -> error "resizeChunksD: Incomplete block"
    step gst (RFooter st buf) = do
        -- Warn if len > footerSize
        let len = Array.byteLength buf
        if len < footerSize_
        then do
            r <- step0 gst st
            case r of
                StreamD.Yield arr st1 -> do
                    arr1 <- Array.splice buf arr
                    return $ StreamD.Skip $ RFooter st1 arr1
                StreamD.Skip st1 -> return $ StreamD.Skip $ RFooter st1 buf
                StreamD.Stop -> error "resizeChunksD: Incomplete footer"
        else do
            res <- liftIO $ validateFooter_ buf
            if res
            then return StreamD.Stop
            else error "resizeChunksD: Invalid footer"
    step _ RDone = return StreamD.Stop

{-# ANN type DecompressState Fuse #-}
data DecompressState st ctx prev
    = DecompressInit st
    | DecompressDo st ctx prev
    | DecompressDone ctx

-- | This combinator assumes all the arrays in the incoming stream are properly
-- resized.
--
-- This combinator works well with untouched arrays compressed with
-- 'compressChunksD'.  A random compressed stream would first need to be
-- resized properly with 'resizeChunksD'.
--
{-# INLINE_NORMAL decompressChunksRawD #-}
decompressChunksRawD ::
       MonadIO m
    => BlockConfig
    -> StreamD.Stream m (Array.Array Word8)
    -> StreamD.Stream m (Array.Array Word8)
decompressChunksRawD cfg (StreamD.Stream step0 state0) =
    StreamD.Stream step (DecompressInit state0)

   where

    {-# INLINE_LATE step #-}
    step _ (DecompressInit st) =
        liftIO
            $ do
                lz4Ctx <- c_createStreamDecode
                return $ StreamD.Skip $ DecompressDo st lz4Ctx Nothing
    step _ (DecompressDone lz4Ctx) =
        liftIO $ c_freeStreamDecode lz4Ctx >> return StreamD.Stop
    step gst (DecompressDo st lz4Ctx prev) = do
        r <- step0 gst st
        case r of
            StreamD.Yield arr st1 -> do
                arr1 <- liftIO $ decompressChunk cfg lz4Ctx arr
                -- Instead of the input array chunk we need to hold the output
                -- array chunk here.
                return $ StreamD.Yield arr1 (DecompressDo st1 lz4Ctx (Just arr1))
            StreamD.Skip st1 ->
                return $ StreamD.Skip $ DecompressDo st1 lz4Ctx prev
            StreamD.Stop -> return $ StreamD.Skip $ DecompressDone lz4Ctx

decompressChunksWithD ::
       (MonadThrow m, MonadIO m)
    => Parser.Parser Word8 m (BlockConfig, FrameConfig)
    -> StreamD.Stream m (Array.Array Word8)
    -> StreamD.Stream m (Array.Array Word8)
decompressChunksWithD p s =
    Stream.concatMapM (\() -> generator) (Stream.fromPure ())

    where

    generator = do
       -- ((cfg, config), next) <- ArrayStream.runArrayFoldBreak (ArrayFold.fromParserD p) (Stream.toStreamK s)
        (res, next) <- ArrayStream.runArrayFoldBreak (ArrayFold.fromParserD p) (Stream.toStreamK s)
        case res of
            Left (Parser.ParseError err) -> error $ "parser error" ++ err
            Right (cfg, config) ->
                return $ decompressChunksRawD cfg (resizeChunksD cfg config (Stream.fromStreamK next))

-- XXX Merge this with BlockConfig?
data FLG =
    FLG
        { isBlockIndependent :: Bool
        , hasBlockChecksum :: Bool
        , hasContentSize :: Bool
        , hasContentChecksum :: Bool
        , hasDict :: Bool
        }

-- XXX Support Skippable frames
simpleFrameParserD ::
       (Monad m, MonadThrow m)
    => Parser.Parser Word8 m (BlockConfig, FrameConfig)
simpleFrameParserD = do
    _ <- assertMagic
    _flg <- parseFLG
    blockMaxSize <- parseBD
    _ <- assertHeaderChecksum
    let config =
            (BlockConfig {blockSize = blockMaxSize}
            , FrameConfig {hasEndMark = True})
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
