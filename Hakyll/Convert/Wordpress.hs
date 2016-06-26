{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Convert.Wordpress
    (readPosts, distill)
  where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as B
import           Data.Maybe
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Time              (UTCTime)
import           Data.Time.Format       (parseTimeM, formatTime, defaultTimeLocale, rfc822DateFormat)
import           Text.XML.Light
import           Text.RSS.Import
import           Text.RSS.Syntax

import           Hakyll.Convert.Common

-- | Returns only public posts
readPosts :: FilePath -> IO (Maybe [RSSItem])
readPosts f = do
    fmap select . parseRssDoc <$> B.readFile f
  where
    parseRssDoc x = elementToRSS =<< parseXMLDoc (T.decodeUtf8 x)
    select        = filter isPublished  .  rssItems . rssChannel

isPublished :: RSSItem -> Bool
isPublished i = "publish" `elem` getStatus i

distill :: RSSItem -> DistilledPost
distill item = DistilledPost
    { dpTitle = T.pack <$> rssItemTitle item
    , dpBody  = content
    , dpUri   = link
    , dpTags  = tags
    , dpCategories = categories
    , dpDate  = date
    }
  where
    link = fromMaybe "" (rssItemLink item)
    content = T.pack
            $ unlines (map strContent contentTags)
    categories = rssCategoriesOfType "category"
    tags       = rssCategoriesOfType "post_tag"
    contentTags = concatMap (findElements contentTag)
        (rssItemOther item)
    rssCategoriesOfType ty =
        [ T.pack (rssCategoryValue c)
        | c <- rssItemCategories item
        , rssCategoryDomain c == Just ty ]
    contentTag = QName
        { qName   = "encoded"
        , qURI    = Just "http://purl.org/rss/1.0/modules/content/"
        , qPrefix = Just "content"
        }
    --
    date = case parseTime' =<< rssItemPubDate item of
               Nothing -> "1970-01-01"
               Just d  -> T.pack (formatTime' d)
    parseTime' d = msum $ map (\f -> parseTimeM True defaultTimeLocale f d)
        [ rfc822DateFormat
        ]
    formatTime' :: UTCTime -> String
    formatTime' = formatTime defaultTimeLocale "%FT%TZ" --for hakyll

-- ---------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------

getStatus :: RSSItem -> [String]
getStatus item =
    map strContent statusTags
  where
    statusTags = concatMap (findElements (wpName "status"))
        (rssItemOther item)
    wpName n = QName
        { qName   = n
        , qURI    = Just "http://wordpress.org/export/1.2/"
        , qPrefix = Just "wp"
        }
