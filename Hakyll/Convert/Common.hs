{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Hakyll.Convert.Common where

import           Data.Data
import           Data.Text                    (Text)
import qualified Data.Text                    as T

data DistilledPost = DistilledPost
    { dpUri   :: String
    , dpBody  :: Text
    , dpTitle :: Maybe Text
    , dpTags  :: [Text]
    , dpDate  :: Text
    }
  deriving (Show, Data, Typeable)
