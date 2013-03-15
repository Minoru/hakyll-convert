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

import           Hakyll.Convert.Blogger

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
    writeFile fname $ unlines
        [ "---"
        , metadata "title"     (dpTitle post)
        , metadata "published" (dpDate post)
        , metadata "tags"      (intercalate ", " (dpTags post))
        , "---"
        , ""
        , dpBody post
        ]
  where
    metadata k v = k ++ ": " ++ v
    -- feed  = toMiniFeed post
    odir  = outputDir cfg
    -- carelessly assumes we can treat URIs like filepaths
    fname = odir </> dropExtensions (chopUri (dpUri post)) <.> "markdown"
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

dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix (x:xs) (y:ys) | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)
