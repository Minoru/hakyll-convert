module Golden.Wordpress where

import Data.Maybe (listToMaybe)
import Golden.GoldenTestHelpers
import Hakyll.Convert.Wordpress
import Test.Tasty (TestTree, testGroup)

goldenTests :: TestTree
goldenTests =
  testGroup
    "Wordpress"
    [ wordpress_000,
      wordpress_001
    ]

wordpress_000 :: TestTree
wordpress_000 =
  testGroup
    "readPosts"
    [ helper
        ("post No. " ++ no)
        (listToMaybe . (drop offset))
      | (no, offset) <- map (\n -> (show n, n)) [0 .. 11]
    ]
  where
    helper = readPostsHelper readPosts "test/golden/data/wordpress-000/"

wordpress_001 :: TestTree
wordpress_001 =
  testGroup
    "distilled posts"
    [ helper
        ("post No. " ++ no)
        (listToMaybe . (drop offset))
      | (no, offset) <- map (\n -> (show n, n)) [0 .. 11]
    ]
  where
    helper = readAndDistillHelper readPosts distill "test/golden/data/wordpress-001/"
