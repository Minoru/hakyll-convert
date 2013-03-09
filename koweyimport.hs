{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Control.Applicative
import           Control.Arrow
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

-- drafts vs posted?
-- comments vs posts
-- comments linked to posts
-- schemes
-- settings vs crap (can be done with id or category)

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
        Just fd -> handleFeed config fd

handleFeed config fd = do
    createDirectoryIfMissing False (outputDir config)
    mapM (savePost config) $ blocks
  where
    blocks = mapMaybe postAndComments
           $ buckets getPost $ filter isInteresting
           $ feedEntries fd

-- | Given a list of entries in which exactly one of the entries
--   is a post and the rest are comments, return a tuple of the
--   post and its comemnts
postAndComments (u,xs) =
    case partition isPost xs of
        ([p],cs) -> Just (p, cs)
        ([],_)   -> Nothing -- spam comment(s) only?
        _        -> error $ "Block with more than one post: " ++ u
                          ++ concatMap displayEntry xs
  where
    isPost = any (isBloggerCategoryOfType "post") . entryCategories

-- | Save a post along with its comments as a mini atom feed
savePost cfg (post, cmts) = do
    putStrLn fname
    writeFile (odir </> fname) $ ppElement $ xmlFeed feed
  where
    feed  = feed_ { feedEntries = post : cmts }
    feed_ = nullFeed (entryId post) (entryTitle post) (fromMaybe "" $ entryPublished post)
    odir  = outputDir cfg
    fname = mkHakyllFilename post

-- | Return a string identifying the post that goes with the entry
--   If it's a comment, it should identify the post the comment is
--   for. If the entry is a post, it should identify the post itself.
getPost e =
    case (getLink isAlt, getLink isSelf) of
        (Just l,  _)       -> postUrl l
        (Nothing, Just l)  -> postUrl l
        (Nothing, Nothing) -> error "don't know what with entry that has no alt/self"
  where
    getLink f = case filter f $ entryLinks e of
                    []  -> Nothing
                    [x] -> Just x
                    xs  -> error $ "More than link of desired type for " ++ txtToString (entryTitle e)
    postUrl = takeWhile (/= '?') . linkHref
    isAlt  l = linkRel l == Just (Right "alternate")
    isSelf l = linkRel l == Just (Right "self")



displayEntry e = unlines
    [ "-----------------------------------------------------------------"
    , "#" ++ txtToString (entryTitle e)
    , "Id: " ++ entryId e
    , "Published:   " ++ maybe "N/A" id (entryPublished e)
    , "In-Reply-To: " ++ show (entryInReplyTo e)
    , "Categories:  " ++
         (intercalate "," . map catTerm . filter (not . isBloggerCategory)
          $ entryCategories e)
    , "Hakyll name: " ++ mkHakyllFilename e
    , ""
    , if interesting
         then take 2000 $ showContent (entryContent e)
         else "-(boring)-"
    ]
  where
    interesting = isInteresting e
    showContent (Just (TextContent x)) = x
    showContent (Just (HTMLContent x)) = x
    showContent x = show x

mkHakyllFilename e =
    intercalate "-" pieces <.> "xml"
  where
    pieces = filter (not . null) $ [ dateStamp, mungedTitle ]
    dateStamp   = maybe "date-unknown" (takeWhile (/= 'T')) $ entryPublished e
    mungedTitle = reverse . dropWhile (== '_') . reverse
                . intercalate "-"
                . filter (not. null)
                . map squish . words . txtToString $ entryTitle e
    squish =
        concatMap squishOne
      where
        squishOne x = if isAlphaNum x then [toLower x] else ['_']

isBloggerCategory = (== Just "http://schemas.google.com/g/2005#kind")
                  . catScheme

isBloggerCategoryOfType ty c =
    isBloggerCategory c &&
    catTerm c == "http://schemas.google.com/blogger/2008/kind#" ++ ty

isInteresting e =
    not $ any isBoring cats
  where
    isBoring c = any (\t -> isBloggerCategoryOfType t c) ["settings", "template"]
    cats       = entryCategories e

readAtomFile f = do
    parseAtomDoc <$> B.readFile f
  where
    parseAtomDoc x = elementFeed =<< parseXMLDoc x

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))


{-
-- | Complain if any of the specified arguments are missing
--   Use a somewhat compact output to make it a bit more readable
--
--   This is useful if you are abusing flags to get
--   non-positional-but-mandatory arguments
dieOnMissingArgs :: String                     -- ^ program name
                 -> config                     -- ^ arguments (eg. cmdargs struct)
                 -> [ (config -> Bool, Text) ] -- ^ (isMissing, description)
                 -> IO ()
dieOnMissingArgs pname config missing_ =
    unless (null missing) $ die . T.unlines $
        [ "I need" <+> commas "and" missing
        , "Try" <+> T.pack pname <+> " --help" -- FIXME: don't know how to print this automatically
        ]
  where
    missing = [ n | (f,n) <- missing_, f config ]
-}
