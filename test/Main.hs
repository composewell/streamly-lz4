module Main (main) where

import Data.Word (Word8)
import Data.Function ((&))
import System.IO (IOMode(..), openFile, hClose)
import System.Directory (getCurrentDirectory)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Property, forAll, property)
import Test.QuickCheck.Gen (Gen, choose, chooseAny, listOf)
import Test.QuickCheck.Monadic (monadicIO)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.FileSystem.Handle as H
import qualified Streamly.Internal.Memory.ArrayStream as AS

import Streamly.Compression.LZ4

cantrbry_alice29_txt :: String -> String
cantrbry_alice29_txt base = base ++ "cantrbry/alice29.txt"

genArrayW8List :: Gen [A.Array Word8]
genArrayW8List = listOf $ A.fromList <$> listOf chooseAny

identitySimple :: Property
identitySimple =
    forAll ((,) <$> genArrayW8List <*> choose (-10, 10))
        $ \(lst, i) ->
              let strm = D.fromList lst
               in monadicIO
                      $ do
                          lst1 <-
                              D.toList $ decompressResizedD $ compressD i strm
                          return $ lst == lst1

identityRealWorld :: Int -> String -> IO ()
identityRealWorld i f = do
    let f1Strm =
            S.bracket_ (openFile f ReadMode) hClose
                $ \h ->
                      S.unfold H.readChunks h & compress i & decompressResized
                          & AS.concat
    f1 <- S.toList f1Strm
    f2 <-
        S.toList
            $ S.bracket_ (openFile f ReadMode) hClose $ \h -> S.unfold H.read h
    f1 `shouldBe` f2

main :: IO ()
main = do
    base <- getCurrentDirectory
    let corpora = base ++ "/corpora/"
    hspec
        $ do
            describe "QuickCheck"
                $ do
                    it "decompressResizedD . compressD == id"
                        $ property $ identitySimple
            describe "Corpora"
                $ do
                    it
                        ("decompressResized . compress == id "
                             ++ cantrbry_alice29_txt "(" ++ ")")
                        $ identityRealWorld 5 (cantrbry_alice29_txt corpora)
