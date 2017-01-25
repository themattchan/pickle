module Pickle.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Either

import System.Directory
import System.FilePath.Posix

import Text.Pandoc
import Text.Pandoc.Error
import qualified Data.Text.Lazy as T

import Pickle.Config

--------------------------------------------------------------------------------
-- * Pickle monad

type Pickle = EitherT PickleError IO

runPickle :: Pickle a -> IO a
runPickle = fmap (either (error . show) id) . runEitherT

pickleAssertIO :: (a -> IO Bool) -> a -> Pickle ()
pickleAssertIO t a = do
  p <- liftIO $ t a
  if p then right () else left FileNotFound

pickleAssert :: (a -> Bool) -> a -> Pickle ()
pickleAssert t a = if t a then right () else left FileNotFound -- FIXME

--------------------------------------------------------------------------------
-- * Errors

data PickleError
  = UnknownError
  | FileNotFound
  | UnknownPropertyError
  | PicklePandocError PandocError
  deriving (Show)

instance Monoid PickleError where
  mempty = UnknownError
  a `mappend` _ = a

--------------------------------------------------------------------------------
-- * Posts

-- | Structure for posts
data Post = Post
  { postContent       :: Pandoc
  -- ^ the pandoc
  , postName          :: FilePath
  -- ^ source file name
  , postOutName       :: Maybe FilePath
  -- ^ out file name, if different
  , postSrcBundle     :: Maybe (FilePath, [FilePath])
  -- ^ if it is in a folder, save the folder name for copying assets
  -- TODO: maybe use the 'MediaBag' from Pandoc?
  , postDate :: String
--  , postMeta          :: PostMeta
  } deriving Show

-- | Metadata for posts
data PostMeta = PostMeta
  { postMetaFilename   :: FilePath
  -- ^ full location in output
  , postMetaDstPath   :: FilePath
  -- ^ calculated from metadata (relative to generated site)
  } deriving Show
