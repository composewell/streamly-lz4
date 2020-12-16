module Main (main) where

import Data.Word (Word8)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Property, forAll, property)
import Test.QuickCheck.Gen (Gen, choose, chooseAny, listOf)
import Test.QuickCheck.Monadic (monadicIO)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Streamly.Compression.LZ4

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

main :: IO ()
main =
    hspec
        $ describe "StreamD"
        $ do
            it "decompressResizedD . compressD == id"
                $ property $ identitySimple
