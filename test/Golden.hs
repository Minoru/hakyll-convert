module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Hakyll.Convert.Blogger as Blogger
import Hakyll.Convert.Wordpress as Wordpress

main :: IO ()
main = defaultMain goldenTests

testHelper ::
  Show a =>
  (FilePath -> IO (Maybe [a]))
  -> String
  -> String
  -> ([a] -> Maybe a)
  -> TestTree
testHelper fut dir testName selector =
  goldenVsString
    testName
    (dir ++ (map spaceToDash testName) ++ ".golden")
    (do
      posts <- fut (dir ++ "input.xml")
      return $
        fromMaybe
          LBS.empty
          (posts
            >>= selector
            >>= (return . LBS.pack . show))
      )
  where
  spaceToDash = \c -> if c == ' ' then '-' else c

goldenTests :: TestTree
goldenTests =
  testGroup "Golden tests for `readPosts`"
    [ blogger
    , wordpress
    ]

{-
 - BLOGGER
 -}

blogger :: TestTree
blogger =
  testGroup "Blogger"
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
  helper = testHelper Blogger.readPosts "test/data/blogger-000/"

{-
 - WORDPRESS
 -}

wordpress :: TestTree
wordpress =
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
  helper = testHelper Wordpress.readPosts "test/data/wordpress-000/"
