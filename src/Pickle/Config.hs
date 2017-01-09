module Pickle.Config where

import System.Directory
import System.FilePath.Posix

data PostHierarchy = YearMonth
                   | YearMonthDay
                   | Flat
                   | Category
                   deriving (Eq, Show, Bounded, Enum)

data SiteConfig = SiteConfig
  { configSiteDir  :: FilePath
  -- ^ the folder in the root of this directory where my site is
  , configPostsDir :: FilePath
  -- ^ relative to 'configSiteDir'
  , configOutDir   :: FilePath
  -- ^ relative to 'configSiteDir'
  , configPostsOutDir :: Maybe FilePath
  -- ^ unused for now
  , configPostsOutFormat :: PostHierarchy
  -- ^ how the posts should be structured,
  -- e.g. YearMonth  /blog/2016/04/my-great-post/index.html
  --      Category   /blog/haskell/haskell-for-category-theorists/index.html

  , configHtmlTemplate :: Maybe String
  -- ^ initialise this yourself, also multiple templates and composition is not supported
  } deriving Show

defaultConfig :: SiteConfig
defaultConfig = SiteConfig
  { configSiteDir        = "site"
  , configPostsDir       = "posts"
  , configOutDir         = "html"
  , configPostsOutDir    = Nothing
  , configPostsOutFormat = YearMonth
  , configHtmlTemplate   = Nothing
  }
