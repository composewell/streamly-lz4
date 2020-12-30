module Main (main) where

import Control.Monad.IO.Class (MonadIO(..))
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
import qualified Streamly.Internal.FileSystem.Handle as H
import qualified Streamly.Internal.Memory.ArrayStream as AS

import Streamly.LZ4

cantrbry_alice29_txt :: String -> String
cantrbry_alice29_txt base = base ++ "cantrbry/alice29.txt"

genArrayW8List :: Gen [A.Array Word8]
genArrayW8List = listOf $ A.fromList <$> listOf chooseAny

decompressResizedcompressSimple :: Property
decompressResizedcompressSimple =
    forAll ((,) <$> genArrayW8List <*> choose (-1, 10))
        $ \(lst, i) ->
              let strm = S.fromList lst
               in monadicIO
                      $ do
                          lst1 <- S.toList $ decompressResized $ compress i strm
                          return $ lst == lst1

decompressResizedcompress :: String -> Property
decompressResizedcompress f =
    forAll (choose (-1, 10))
        $ \i ->
              monadicIO
                  $ liftIO
                  $ do
                      let f1Strm =
                              S.bracket_ (openFile f ReadMode) hClose
                                  $ \h ->
                                        S.unfold
                                            H.readChunksWithBufferOf (300, h)
                                                & compress i
                                                & decompressResized
                                                & AS.concat
                      f1 <- S.toList f1Strm
                      f2 <-
                          S.toList
                              $ S.bracket_ (openFile f ReadMode) hClose
                              $ \h -> S.unfold H.read h
                      f1 `shouldBe` f2

decompressCompress :: String -> Property
decompressCompress f =
    forAll (choose (-1, 10))
        $ \i ->
              monadicIO
                  $ liftIO
                  $ do
                      let tmp = "/tmp/test.lz4"
                          openers = do
                              r <- openFile f ReadMode
                              w <- openFile tmp WriteMode
                              return (r, w)
                          closers (r, w) = hClose r >> hClose w
                      (r, w) <- openers
                      S.unfold H.readChunksWithBufferOf (300, r)
                          & compress i
                          & H.fromChunks w
                      closers (r, w)
                      f1 <-
                          S.toList
                              $ S.bracket_ (openFile tmp ReadMode) hClose
                              $ \h ->
                                    S.unfold H.readChunksWithBufferOf (50, h)
                                        & decompress
                                        & AS.concat
                      f2 <-
                          S.toList
                              $ S.bracket_ (openFile f ReadMode) hClose
                              $ \h -> S.unfold H.read h
                      f1 `shouldBe` f2

main :: IO ()
main = do
    base <- getCurrentDirectory
    let corpora = base ++ "/corpora/"
    hspec
        $ do
            describe "Identity"
                $ do
                    it "decompressResized . compress == id"
                        $ property $ decompressResizedcompressSimple
                    it
                        ("decompressResized . compress == id "
                             ++ cantrbry_alice29_txt "(" ++ ")")
                        $ decompressResizedcompress
                        $ cantrbry_alice29_txt corpora
                    it
                        ("decompress . compress == id "
                             ++ cantrbry_alice29_txt "(" ++ ")")
                        $ decompressCompress $ cantrbry_alice29_txt corpora
