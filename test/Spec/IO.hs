{-# LANGUAGE OverloadedStrings  #-}

module Spec.IO (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Data.DateTime (fromGregorian)
import Data.Default (def)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Hakyll.Convert.Common (DistilledPost(..))
import Hakyll.Convert.IO (savePost)

tests :: TestTree
tests = testGroup "IO.savePost"
  [ namesTheFileAccordingToFormat
  ]

namesTheFileAccordingToFormat :: TestTree
namesTheFileAccordingToFormat =
  testCase
    "Names the output file according to the given filename format"
    (withSystemTempDirectory "hakyll-convert" $ \tempDir -> do
      let output_format = "%o-%Y^%y%%_%s%dd & %m—%S%H%M"
      let file_extension = "xyz"
      let post = def {
          -- The slug, %s, is going to be "yet-another"
          dpUri = "https://example.com/2020/yet-another.post.html"
        , dpDate = fromGregorian 2020 11 6 11 33 46
        }
      savePost tempDir output_format file_extension post

      let filename = tempDir </> "2020/yet-another-2020^20%_yet-another06d & 11—461133.xyz"
      exists <- doesFileExist filename
      assertBool "The file with expected name doesn't exist" exists)
