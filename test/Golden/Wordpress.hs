module Golden.Wordpress where

import Data.Maybe (listToMaybe)
import Test.Tasty (TestTree, testGroup)

import Hakyll.Convert.Wordpress

import Golden.GoldenTestHelpers

goldenTests :: TestTree
goldenTests =
  testGroup "Wordpress"
    [ wordpress_00
    ]

wordpress_00 :: TestTree
wordpress_00 =
  testGroup "wordpress-000"
    [ helper
        ("post No. " ++ no)
        (listToMaybe . (drop offset))
    | (no, offset) <- map (\n -> (show n, n)) [0..11]
    ]
  where
  helper = conversionHelper readPosts "test/data/wordpress-000/"
