module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Golden.Blogger as Blogger
import qualified Golden.IO as IO
import qualified Golden.Wordpress as Wordpress

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests =
  testGroup "Golden tests"
    [ Blogger.goldenTests
    , IO.goldenTests
    , Wordpress.goldenTests
    ]
