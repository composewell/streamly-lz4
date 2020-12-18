module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Data.Function ((&))
import System.IO (IOMode(..), openFile, hClose)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (forAll, property)
import Test.QuickCheck.Gen
    ( Gen, choose, chooseAny, elements, generate
    , listOf, vectorOf
    )
import Test.QuickCheck.Monadic (monadicIO)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.FileSystem.Handle as H
import qualified Streamly.Internal.Memory.ArrayStream as AS

import Streamly.LZ4

genArrayW8List :: Gen [A.Array Word8]
genArrayW8List = listOf $ A.fromList <$> listOf chooseAny

genArrayW8ListLarge :: Gen [A.Array Word8]
genArrayW8ListLarge = do
    let minArr = 1024 * 10
        maxArr = 1024 * 100
        minVec = 50
        maxVec = 100
    arrS <- choose (minArr, maxArr)
    vecS <- choose (minVec, maxVec)
    let arrGen = A.fromList <$> vectorOf arrS chooseAny
    vectorOf vecS arrGen

genAcceleration :: Gen Int
genAcceleration = elements [-1..12]

decompressResizedcompress :: (Int, [A.Array Word8]) -> IO ()
decompressResizedcompress (i, lst) =
    let strm = S.fromList lst
     in do lst1 <- S.toList $ decompressResized $ compress i strm
           lst `shouldBe` lst1

decompressCompress :: (Int, [A.Array Word8]) -> IO ()
decompressCompress (i, lst) = do
    let tmp = "/tmp/test.lz4"
        strm = S.fromList lst
    w <- openFile tmp WriteMode
    compress i strm & H.fromChunks w
    hClose w
    f1 <-
        S.toList
            $ S.bracket_ (openFile tmp ReadMode) hClose
            $ \h -> S.unfold H.readChunks h & decompress & AS.concat
    f2 <- S.toList $ AS.concat strm
    f1 `shouldBe` f2

main :: IO ()
main = do
    large <- generate genArrayW8ListLarge
    hspec
        $ describe "Identity"
        $ propsSimple >> forM_ [-1, 5, 12, 100] (\a -> propsBig (a, large))

    where

    propsSimple = do
        it "decompressResized . compress == id"
            $ property
            $ forAll ((,) <$> genAcceleration <*> genArrayW8List)
            $ monadicIO . liftIO . decompressResizedcompress
        it "decompress . compress == id"
            $ property
            $ forAll ((,) <$> genAcceleration <*> genArrayW8List)
            $ monadicIO . liftIO . decompressCompress

    propsBig r@(i, _) = do
        it ("decompressResized . compress (" ++ show i ++ ") == id (big)")
            $ decompressResizedcompress r
        it ("decompress . compress (" ++ show i ++ ") == id (big)")
            $ decompressCompress r
