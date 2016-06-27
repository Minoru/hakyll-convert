{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Hakyll.Convert.Common where

import           Data.Data
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)

data DistilledPost = DistilledPost
    { dpUri   :: String
    , dpBody  :: Text
    , dpTitle :: Maybe Text
    , dpTags  :: [Text]
    -- | Categories are coarser-grained than tags; you might be
    --   interested in ignoring tags and just focusing on categories
    --   in cases where you have lots of little uninteresting tags.
    , dpCategories :: [Text]
    , dpDate  :: UTCTime
    }
  deriving (Data, Typeable)
