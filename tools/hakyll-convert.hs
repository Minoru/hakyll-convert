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
import           Text.Atom.Feed
import           Text.Atom.Feed.Export
import           Text.Atom.Feed.Import
import           Text.XML.Light

import           Hakyll.Convert.Blogger
import           Hakyll.Convert.Common

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
    mapM (savePost config) $ map distill $ extractPosts (feedEntries fd)

-- ---------------------------------------------------------------------
-- From Blogger
-- How Blogger represents posts, comments, etc in Atom
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- To Hakyll (sort of)
-- Saving feed in bite-sized pieces
-- ---------------------------------------------------------------------

-- | Save a post along with its comments as a mini atom feed
savePost :: Config -> DistilledPost -> IO ()
savePost cfg post = do
    putStrLn fname
    createDirectoryIfMissing True (takeDirectory fname)
    B.writeFile fname . T.encodeUtf8 $ T.unlines
        [ "---"
        , metadata "title"     (formatTitle (dpTitle post))
        , metadata "published" (formatDate  (dpDate  post))
        , metadata "tags"      (formatTags  (dpTags  post))
        , "---"
        , ""
        , dpBody post
        ]
  where
    metadata k v = k <> ": " <> v
    odir  = outputDir cfg
    --
    fname    = odir </> postPath <.> "html"
    postPath = dropExtensions (chopUri (dpUri post))
      where
        chopUri (dropPrefix "http://" -> ("",rest)) =
           -- carelessly assumes we can treat URIs like filepaths
           joinPath $ drop 1 $ splitPath rest -- drop the domain
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

-- ---------------------------------------------------------------------
-- utilities
-- ---------------------------------------------------------------------

readAtomFile f = do
    parseAtomDoc <$> B.readFile f
  where
    parseAtomDoc x = elementFeed . deleteDrafts =<< parseXMLDoc (T.decodeUtf8 x)

dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix (x:xs) (y:ys) | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)
