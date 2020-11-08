{-# LANGUAGE StandaloneDeriving #-}

module Golden.GoldenTestHelpers where

import Data.Maybe (fromMaybe)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Hakyll.Convert.Common

deriving instance Show DistilledPost

readPostsHelper ::
  Show a =>
  (FilePath -> IO (Maybe [a]))
  -> String
  -> String
  -> ([a] -> Maybe a)
  -> TestTree
readPostsHelper readPosts dir testName selector =
  goldenVsString
    testName
    (dir ++ (map spaceToDash testName) ++ ".golden")
    (do
      posts <- readPosts (dir ++ "input.xml")
      return $
        fromMaybe
          LBS.empty
          (posts
            >>= selector
            >>= (return . LBS.pack . show))
      )
  where
  spaceToDash = \c -> if c == ' ' then '-' else c

readAndDistillHelper ::
  (FilePath -> IO (Maybe [a]))
  -> (Bool -> a -> DistilledPost)
  -> String
  -> String
  -> ([DistilledPost] -> Maybe DistilledPost)
  -> TestTree
readAndDistillHelper readPosts distill dir testName selector =
  let fut filepath = do
        posts <- readPosts filepath
        let extractComments = True
        return $ fmap (\items -> map (distill extractComments) items) posts
  in readPostsHelper fut dir testName selector
