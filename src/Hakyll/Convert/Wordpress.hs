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

distill :: Bool -> RSSItem -> DistilledPost
distill extractComments item = DistilledPost
    { dpTitle = T.pack <$> rssItemTitle item
    , dpBody  = body
    , dpUri   = link
    , dpTags  = tags
    , dpCategories = categories
    , dpDate  = date
    }
  where
    body =
        if extractComments
        then T.intercalate "\n"
                           [ content
                           , ""
                           , "<h3 id='hakyll-convert-comments-title'>Comments</h3>"
                           , comments
                           ]
        else content
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
    comments = T.intercalate "\n" $ map formatComment $ commentTags
    commentTags = rssItemOther item >>= findElements commentTag
    commentTag = wordpressTag "comment"
    --
    date = case parseTime' =<< rssItemPubDate item of
               Nothing -> fromJust $ parseTime' "Thu, 01 Jan 1970 00:00:00 UTC"
               Just  d -> d
    parseTime' d = msum $ map (\f -> parseTimeM True defaultTimeLocale f d)
        [ rfc822DateFormat
        ]

-- ---------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------

formatComment :: Element -> T.Text
formatComment commentElement =
    T.intercalate "\n" [
          "<div class='hakyll-convert-comment'>"
         , T.concat [ "<p class='hakyll-convert-comment-date'>"
                    , "On ", pubdate, ", ", author, " wrote:"
                    , "</p>" ]
         , "<div class='hakyll-convert-comment-body'>", comment, "</div>"
         , "</div>"
         ]
    where pubdate = T.pack $ fromMaybe "unknown date" $ findField "comment_date"
          author = T.pack $ fromMaybe "unknown author" $ findField "comment_author"
          comment = T.pack $ fromMaybe "" $ findField "comment_content"
          findField name =
              strContent <$> findChild (wordpressTag name) commentElement

wordpressTag :: String -> QName
wordpressTag name =
    QName
    { qName = name
    , qURI = Just "http://wordpress.org/export/1.2/"
    , qPrefix = Just "wp"
    }

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
