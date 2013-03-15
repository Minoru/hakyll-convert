{-# LANGUAGE DeriveDataTypeable #-}

module Hakyll.Convert.Blogger where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Binary
import           Data.Char
import           Data.Data
import           Data.Function
import           Data.List
import           Data.List
import           Data.Maybe
import           Data.Monoid

import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Template.Context
import           Text.Atom.Feed
import           Text.Atom.Feed.Export
import           Text.Atom.Feed.Import
import           Text.XML.Light

-- | A post and its comments
data FullPost = FullPost
    { fpPost     :: Entry
    , fpComments :: [Entry]
    , fpUri      :: String
    }

data DistilledPost = DistilledPost
    { dpUri   :: String
    , dpBody  :: String
    , dpTitle :: String
    , dpTags  :: [String]
    , dpDate  :: String
    }
  deriving (Show, Data, Typeable)

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

-- | A mini feed is a small Atom feed whose first entry is a blog post
--   and whose subsequent entries are comments
fromMiniFeed :: Feed -> FullPost
fromMiniFeed fd =
    fromMaybe (error oops) (fromMiniFeed_ fd)
  where
    oops = "Not a hakyll-convert minifeed"

fromMiniFeed_ :: Feed -> Maybe FullPost
fromMiniFeed_ fd =
    case extractPosts (feedEntries fd) of
        [x] -> Just x
        _   -> Nothing

toMiniFeed :: FullPost -> Feed
toMiniFeed post =
    feed_ { feedEntries = fpPost post : fpComments post }
  where
    mainPost = fpPost post
    feed_ = nullFeed (entryId mainPost)
                     (entryTitle mainPost)
                     (fromMaybe "" $ entryPublished mainPost)

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

-- Reads mini Atom feed generated by Hakyll convert
bloggerCompiler :: Compiler (Item DistilledPost)
bloggerCompiler =
    withItemBody unwrap =<<
    (fmap parseAtomDoc <$> getResourceBody)
  where
    unwrap Nothing  = fail "Could not understand feed"
    unwrap (Just f) = return (distill (fromMiniFeed f))

feedPrinter :: Item DistilledPost -> Compiler (Item String)
feedPrinter feedItem = cached cacheName $
    withItemBody (return . dpBody) feedItem
  where
    cacheName = "Hakyll.Convert.Blogger"

parseAtomDoc x = elementFeed =<< parseXMLDoc x

distill :: FullPost -> DistilledPost
distill fp = DistilledPost
    { dpBody  = body fpost
    , dpUri   = fpUri fp
    , dpTitle = title fpost
    , dpTags  = tags fpost
    , dpDate  = date fpost
    }
  where
    fpost = fpPost fp
    --
    body = fromContent . entryContent
    fromContent (Just (HTMLContent x)) = x
    fromContent _ = error "Hakyll.Convert.Blogger.feedPrinter expecting HTML"
    --
    title = txtToString . entryTitle
    tags = map catTerm
         . filter (not . isBloggerCategory)
         . entryCategories
    date x =
        case entryPublished x of
            Nothing -> "1970-01-01"
            Just  d -> d

bloggerContext :: Context DistilledPost
bloggerContext =
    fpfield "date" dpDate `mappend`
    fpfield "body" dpBody `mappend`
    fpfield "tags" (intercalate ", " . dpTags)
  where
    fpfield key f = field key (return . f . itemBody)

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

 
{-!
deriving instance Binary DistilledPost
!-}
-- GENERATED START

 
instance Binary DistilledPost where
        put (DistilledPost x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               return (DistilledPost x1 x2 x3 x4 x5)
-- GENERATED STOP
