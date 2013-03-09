{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import qualified Data.ByteString        as B
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.FilePath

import           System.Console.CmdArgs
import           Text.Atom.Feed
import           Text.Atom.Feed.Export
import           Text.Atom.Feed.Import
import           Text.XML.Light

data Config = Config
    { feed      :: FilePath
    , outputDir :: FilePath
    }
 deriving (Show, Data, Typeable)

parameters :: FilePath -> Config
parameters p = modes
    [ Config
        { feed         = def &= argPos 0 &= typ "ATOM-FILE"
        , outputDir    = def &= argPos 1 &= typDir
        } &= help "Save blog posts Blogger feed into individual posts"
    ] &= program (takeFileName p)

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

main = do
    p      <- getProgName
    config <- cmdArgs (parameters p)
    --
    mfeed <- readAtomFile (feed config)
    case mfeed of
        Nothing -> fail $ "Could not understand Atom feed: " ++ feed config
        Just fd -> processBloggerFeed config fd

processBloggerFeed config fd = do
    mapM (savePost config) $ extractPosts (feedEntries fd)

-- ---------------------------------------------------------------------
-- From Blogger
-- How Blogger represents posts, comments, etc in Atom
-- ---------------------------------------------------------------------

-- | A post and its comments
data FullPost = FullPost
    { fpPost     :: Entry
    , fpComments :: [Entry]
    , fpUri      :: String
    }

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

-- | Contains actual meat (posts, comments; but not eg. templates)
isInteresting :: Entry -> Bool
isInteresting e =
    not $ any isBoring cats
  where
    isBoring c = any (\t -> isBloggerCategoryOfType t c) ["settings", "template"]
    cats       = entryCategories e

-- ---------------------------------------------------------------------
-- To Hakyll (sort of)
-- Saving feed in bite-sized pieces
-- ---------------------------------------------------------------------

-- | Save a post along with its comments as a mini atom feed
savePost :: Config -> FullPost -> IO ()
savePost cfg post = do
    putStrLn fname
    createDirectoryIfMissing True (takeDirectory fname)
    writeFile fname $ ppElement $ xmlFeed feed
  where
    mainPost = fpPost post
    feed  = feed_ { feedEntries = fpPost post : fpComments post }
    feed_ = nullFeed (entryId mainPost) (entryTitle mainPost) (fromMaybe "" $ entryPublished mainPost)
    odir  = outputDir cfg
    -- carelessly assumes we can treat URIs like filepaths
    fname = odir </> dropExtensions (chopUri (fpUri post)) <.> "xml"
    chopUri (dropPrefix "http://" -> ("",rest)) =
        joinPath $ drop 1 $ splitPath rest -- drop the domain
    chopUri u = error $
        "We've wrongly assumed that blog post URIs start with http://, but we got: " ++ u

-- ---------------------------------------------------------------------
-- utilities
-- ---------------------------------------------------------------------

readAtomFile f = do
    parseAtomDoc <$> B.readFile f
  where
    parseAtomDoc x = elementFeed . deleteDrafts =<< parseXMLDoc x

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

entryError e msg =
    error $ msg ++ " [on entry " ++ entryId e ++ "]\n" ++ show e

buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))

dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix (x:xs) (y:ys) | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)
