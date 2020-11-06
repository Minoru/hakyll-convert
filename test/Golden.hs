module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Golden.Blogger as Blogger
import qualified Golden.Wordpress as Wordpress

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests =
  testGroup "Golden tests for `readPosts`"
    [ Blogger.goldenTests
    , Wordpress.goldenTests
    ]
