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
import           Data.Monoid
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           System.Directory
import           System.Environment
import           System.FilePath

import           System.Console.CmdArgs
import           Text.RSS.Export
import           Text.RSS.Import
import           Text.RSS.Syntax
import           Text.Atom.Feed
import           Text.Atom.Feed.Export
import           Text.Atom.Feed.Import
import           Text.XML.Light

import           Hakyll.Convert.Common
import qualified Hakyll.Convert.Blogger   as Blogger
import qualified Hakyll.Convert.Wordpress as Wordpress

data InputFormat = Blogger | Wordpress
  deriving (Data, Typeable, Enum, Show)

data Config = Config
    { feed      :: FilePath
    , outputDir :: FilePath
    , format    :: InputFormat
    }
 deriving (Show, Data, Typeable)

parameters :: FilePath -> Config
parameters p = modes
    [ Config
        { feed         = def &= argPos 0 &= typ "ATOM/RSS FILE"
        , outputDir    = def &= argPos 1 &= typDir
        , format       = Blogger &= help "blogger or wordpress"
        } &= help "Save blog posts Blogger feed into individual posts"
    ] &= program (takeFileName p)

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

main = do
    p      <- getProgName
    config <- cmdArgs (parameters p)
    case format config of
        Blogger   -> mainBlogger   config
        Wordpress -> mainWordPress config

mainBlogger :: Config -> IO ()
mainBlogger config = do
    mfeed <- Blogger.readPosts (feed config)
    case mfeed of
        Nothing -> fail $ "Could not understand Atom feed: " ++ feed config
        Just fd -> mapM_ process fd
  where
    process = savePost config "html" . Blogger.distill

mainWordPress :: Config -> IO ()
mainWordPress config = do
    mfeed <- Wordpress.readPosts (feed config)
    case mfeed of
        Nothing -> fail $ "Could not understand RSS feed: " ++ feed config
        Just fd -> mapM_ process fd
  where
    process = savePost config "markdown" . Wordpress.distill

-- ---------------------------------------------------------------------
-- To Hakyll (sort of)
-- Saving feed in bite-sized pieces
-- ---------------------------------------------------------------------

-- | Save a post along with its comments as a mini atom feed
savePost :: Config -> String -> DistilledPost -> IO ()
savePost cfg ext post = do
    putStrLn fname
    createDirectoryIfMissing True (takeDirectory fname)
    B.writeFile fname . T.encodeUtf8 $ T.unlines
        [ "---"
        , metadata "title"     (formatTitle (dpTitle post))
        , metadata "published" (formatDate  (dpDate  post))
        , metadata "categories" (formatTags (dpCategories post))
        , metadata "tags"      (formatTags  (dpTags  post))
        , "---"
        , ""
        , formatBody (dpBody post)
        ]
  where
    metadata k v = k <> ": " <> v
    odir  = outputDir cfg
    --
    fname    = odir </> postPath <.> ext
    postPath = dropTrailingSlash
             . dropExtensions
             $ chopUri (dpUri post)
      where
        dropTrailingSlash = reverse . dropWhile (== '/') . reverse
        chopUri (dropPrefix "http://" -> ("",rest)) =
           -- carelessly assumes we can treat URIs like filepaths
           joinPath $ drop 1 -- drop the domain
                    $ splitPath rest
        chopUri u = error $
           "We've wrongly assumed that blog post URIs start with http://, but we got: " ++ u
    --
    formatTitle (Just t) = t
    formatTitle Nothing  =
        "untitled (" <> T.unwords firstFewWords <> "…)"
      where
        firstFewWords = T.splitOn "-" . T.pack $ takeFileName postPath
    formatDate  = id
    formatTags  = T.intercalate ","
    formatBody  = id

{-
-- Ugh! convert br tags inside of pre tags
fixupBloggerHtml :: Content -> Content
fixupBloggerHtml = descendElem $ \e ->
    if elName e == unqual "pre"
       then Just . Elem $
                e { elContent = map (descendElem fixBr) (elContent e) }
       else Nothing
  where
    fixBr e =
       if elName e == unqual "br"
          then Just (Text newline)
          else Nothing
    newline = CData CDataRaw "\n" Nothing

descendElem pred (Elem e) =
   case pred e of
       Nothing -> Elem $ e  { elContent = map (descendElem pred) (elContent e) }
       Just e2 -> e2
descendElem _ x = x
-}

-- ---------------------------------------------------------------------
-- utilities
-- ---------------------------------------------------------------------

dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix (x:xs) (y:ys) | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)
