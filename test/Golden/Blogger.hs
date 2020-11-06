module Golden.Blogger where

import Data.Maybe (fromMaybe, listToMaybe)
import Test.Tasty (TestTree, testGroup)

import Hakyll.Convert.Blogger

import Golden.GoldenTestHelpers

goldenTests :: TestTree
goldenTests =
  testGroup "Blogger.readPosts"
    [ blogger_00
    ]

blogger_00 :: TestTree
blogger_00 =
  testGroup "blogger-000"
    [ helper
        ("post No." ++ no)
        (listToMaybe . (drop offset))
    | (no, offset) <- map (\n -> (show n, n)) [0..2]
    ]
  where
  helper = conversionHelper readPosts "test/data/blogger-000/"
