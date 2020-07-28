{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
module Hakyll.Convert.Blogger
    (FullPost(..), readPosts, distill)
  where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString             as B
import           Data.Char
import           Data.Data
import           Data.Function
import           Data.List
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time                    (UTCTime)
import           Data.Time.Format             (parseTimeM, defaultTimeLocale)

import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Template.Context
import           Text.Atom.Feed
import           Text.Atom.Feed.Export
import           Text.Atom.Feed.Import
import           Text.XML.Light

import           Hakyll.Convert.Common

-- | A post and its comments
data FullPost = FullPost
    { fpPost     :: Entry
    , fpComments :: [Entry]
    , fpUri      :: String
    }
  deriving (Show)

-- | An entry is assumed to be either a post, or a comment.
--   If it's a post, it should be associated with the URI
--   that visitors would use to read the post (on the old blog)
--   If it's a comment, it should be the URI for the corresponding
--   post.
data BloggerEntry =
    Post    { beUri_ :: String, beEntry :: Entry }
  | Comment { beUri_ :: String, beEntry :: Entry }
  | Orphan  { beEntry :: Entry }
  deriving (Show)

beUri :: BloggerEntry -> Maybe String
beUri (Orphan _)    = Nothing
beUri (Post u _)    = Just u
beUri (Comment u _) = Just u

-- ---------------------------------------------------------------------
-- Feed to helper type
-- ---------------------------------------------------------------------

-- | Returns only published posts
readPosts :: FilePath -> IO (Maybe [FullPost])
readPosts f = do
    parseAtomDoc <$> B.readFile f
  where
    parseAtomDoc x =
        select =<< parseXMLDoc (T.decodeUtf8 x)
    select =
        fmap (extractPosts . feedEntries) . elementFeed . deleteDrafts

-- has to be done on the XML level as our atom lib doesn't understand
-- the blogger-specific XML for drafts
deleteDrafts :: Element -> Element
deleteDrafts e =
    e { elContent = filter isInnocent (elContent e) }
  where
    isInnocent (Elem e) = not (isDraft e)
    isInnocent _ = True

isDraft :: Element -> Bool
isDraft e =
    isJust $ findElement draft e
  where
    draft = QName
        { qName   = "draft"
        , qURI    = Just "http://purl.org/atom/app#"
        , qPrefix = Just "app"
        }



-- | Warning: this silently ignores orphans, templates, settings
extractPosts :: [Entry] -> [FullPost]
extractPosts entries =
    map toFullPost blocks
  where
    toFullPost (uri, entries) = FullPost
         { fpPost     = post
         , fpComments = comments
         , fpUri      = uri
         }
       where
         post = case [ e | Post _ e <- entries ] of
                    []  -> huh "Block of entries with no post?!"
                    [p] -> p
                    ps  -> huh "Block of entries with more than one post?!"
         comments = [ e | Comment _ e <- entries ]
         huh msg  = error . unlines $ msg : map (txtToString . entryTitle . beEntry) entries
    blocks = [ (u,xs) | (Just u, xs) <- blocks_ ] -- drop orphans
    blocks_ = buckets beUri
            $ map identifyEntry
            $ filter isInteresting entries

-- | Contains actual meat (posts, comments; but not eg. templates)
isInteresting :: Entry -> Bool
isInteresting e =
    not $ any isBoring cats
  where
    isBoring c = any (\t -> isBloggerCategoryOfType t c) ["settings", "template"]
    cats       = entryCategories e


-- | Tag an entry from the blogger feed as either being a post,
--   a comment, or an "orphan" (a comment without an associated post)
identifyEntry :: Entry -> BloggerEntry
identifyEntry e =
    if isPost e
        then case getLink "self" `mplus` getLink "alternate" of
                 Just l  -> Post (postUrl l) e
                 Nothing -> entryError e oopsSelf
        else case getLink "alternate" of
                 Just l  -> Comment (postUrl l) e
                 Nothing -> Orphan e
  where
    isPost  = any (isBloggerCategoryOfType "post") . entryCategories
    postUrl = takeWhile (/= '?') . linkHref
    getLink ty = case filter (isLink ty) $ entryLinks e of
        []  -> Nothing
        [x] -> Just x
        xs  -> entryError e (oopsLink ty)
    isLink ty l = linkRel l == Just (Right ty) && linkType l == Just "text/html"
    oopsSelf    = "Was expecting blog posts to have a self link"
    oopsLink ty = "Was expecting entries have at most one link of type " ++ ty

isBloggerCategory :: Category -> Bool
isBloggerCategory = (== Just "http://schemas.google.com/g/2005#kind")
                  . catScheme

isBloggerCategoryOfType :: String -- ^ \"comment\", \"post\", etc
                        -> Category
                        -> Bool
isBloggerCategoryOfType ty c =
    isBloggerCategory c &&
    catTerm c == "http://schemas.google.com/blogger/2008/kind#" ++ ty

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

distill :: Bool -> FullPost -> DistilledPost
distill extractComments fp = DistilledPost
    { dpBody  = body fpost
    , dpUri   = fpUri fp
    , dpTitle = title fpost
    , dpTags  = tags fpost
    , dpCategories = []
    , dpDate  = date fpost
    }
  where
    fpost     = fpPost fp
    fcomments = fpComments fp
    --
    body post =
      let article = fromContent $ entryContent post
          comments = T.intercalate "\n" $ map formatComment fcomments
      in if extractComments
           then T.intercalate "\n"
                              [ article
                              , ""
                              , "<h3 id='hakyll-convert-comments-title'>Comments</h3>"
                              , comments]
           else article

    fromContent (Just (HTMLContent x)) = T.pack x
    fromContent _ = error "Hakyll.Convert.Blogger.distill expecting HTML"

    formatComment c = T.intercalate "\n" [
        "<div class='hakyll-convert-comment'>"
      , T.concat [ "<p class='hakyll-convert-comment-date'>"
                 , "On ", pubdate, ", ", author, " wrote:"
                 , "</p>" ]
      , "<div class='hakyll-convert-comment-body'>", comment, "</div>"
      , "</div>"
      ]
      where
      pubdate = case entryPublished c of
                    Just d  -> T.pack d
                    Nothing -> "unknown date"
      author = T.unwords $ map (T.pack . personName) (entryAuthors c)
      comment = fromContent $ entryContent c
    --
    title p = case txtToString (entryTitle p) of
         "" -> Nothing
         t  -> Just (T.pack t)
    tags = map (T.pack . catTerm)
         . filter (not . isBloggerCategory)
         . entryCategories
    date x = case parseTime' =<< entryPublished x of
                 Nothing -> fromJust $ parseTime' "1970-01-01T00:00:00Z"
                 Just  d -> d
    parseTime' d = msum $ map (\f -> parseTimeM True defaultTimeLocale f d)
        [ "%FT%T%Q%z"  -- with time zone
        , "%FT%T%QZ"   -- zulu time
        ]

-- ---------------------------------------------------------------------
-- odds and ends
-- ---------------------------------------------------------------------

entryError e msg =
    error $ msg ++ " [on entry " ++ entryId e ++ "]\n" ++ show e

buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))
