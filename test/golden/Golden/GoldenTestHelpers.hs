module Golden.GoldenTestHelpers where

import Data.Maybe (fromMaybe)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as LBS

conversionHelper ::
  Show a =>
  (FilePath -> IO (Maybe [a]))
  -> String
  -> String
  -> ([a] -> Maybe a)
  -> TestTree
conversionHelper fut dir testName selector =
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
