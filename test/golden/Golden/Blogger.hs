module Golden.Blogger where

import Data.Maybe (fromMaybe, listToMaybe)
import Test.Tasty (TestTree, testGroup)

import Hakyll.Convert.Blogger
import Hakyll.Convert.Common

import Golden.GoldenTestHelpers

goldenTests :: TestTree
goldenTests =
  testGroup "Blogger"
    [ blogger_000
    , blogger_001
    ]

blogger_000 :: TestTree
blogger_000 =
  testGroup "readPosts"
    [ helper
        ("post No." ++ no)
        (listToMaybe . (drop offset))
    | (no, offset) <- map (\n -> (show n, n)) [0..2]
    ]
  where
  helper = readPostsHelper readPosts "test/golden/data/blogger-000/"

blogger_001 :: TestTree
blogger_001 =
  testGroup "distilled posts"
    [ helper
        ("post No." ++ no)
        (listToMaybe . (drop offset))
    | (no, offset) <- map (\n -> (show n, n)) [0..2]
    ]
  where
  helper = readAndDistillHelper readPosts distill "test/golden/data/blogger-001/"
