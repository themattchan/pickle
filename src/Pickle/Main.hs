{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pickle.Main where

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

import qualified Text.Blaze.Html4.Strict as B

import qualified Pickle.Config as Config
import Pickle.Files

type Pickle = EitherT PickleError IO

runPickle :: Pickle a -> IO a
runPickle = fmap (either (error . show) id) . runEitherT

data PickleError
  = UnknownError
  | FileNotFound
  | PicklePandocError PandocError
  deriving (Show)

instance Monoid PickleError where
  mempty = UnknownError
  a `mappend` _ = a

pickleAssertIO :: (a -> IO Bool) -> a -> Pickle ()
pickleAssertIO t a = do
  p <- liftIO $ t a
  if p then right () else left FileNotFound

pickleAssert :: (a -> Bool) -> a -> Pickle ()
pickleAssert t a = if t a then right () else left FileNotFound -- FIXME

-- | Extractor methods for the Pandoc type defined in 'Text.Pandoc'
pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

pandocBody :: Pandoc -> [Block]
pandocBody (Pandoc _ b) = b

-- | Names of files containing the text of an article in a folder
contentFileNames :: [String]
contentFileNames = [ "index" ]

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
  } deriving Show

-- | Metadata for posts
data PostMeta = PostMeta
  { postMetaPandocMeta :: Meta
  , postMetaFilename   :: FilePath
  -- ^ full location in output
  , postMetaDstPath   :: FilePath
  -- ^ calculated from metadata (relative to generated site)
  } deriving Show

-- postExtractMeta :: Post -> PostMeta
-- postExtractMeta p@Post{..} = PostMeta
--   { postMetaPandocMeta = pandocMeta postContent
--   , postMetaFilename   = postDstPath </> fromMaybe postName postOutName
--   }

parseAsPandoc :: FilePath -> Pickle Pandoc
parseAsPandoc fp =  EitherT $ first PicklePandocError <$> parsedPd
  where
    reader = hoistEither $ maybeToEither $ getPandocReader =<< getFileFormat fp
    parsedPd = do
      file <- readFile fp
      StringReader r <- reader
      return $ r def file

-- | relative filepaths
readPost :: FilePath -> Pickle Post
readPost fp = readPostFolder <|> readPostSingle
  where
    readPostFolder = do
      pickleAssertIO doesDirectoryExist fp
      (contents, assets) <- lift $ filterContents <$> listDirectory fp
      pickleAssert (not . null) contents
      let postName = fp </> head contents
      postContent <- parseAsPandoc postName
      let postSrcBundle = Just (fp, assets)
          postOutName = Nothing -- TODO
      return Post{..}

    readPostSingle = do
      pickleAssertIO doesFileExist fp
      postContent <- parseAsPandoc fp
      let postName = fp
          postOutName = Nothing -- TODO
          postSrcBundle = Nothing
      return Post{..}

    filterContents = partition ((`elem` contentFileNames) . takeBaseName)

renderPandoc :: Pandoc -> B.Html
renderPandoc = writeHtml def

-- | Write the post to disk, return metadata
writePost :: Post -> Pickle PostMeta
writePost Post{..} = undefined

buildIndex :: [PostMeta] -> B.Html
buildIndex = undefined

-- categories, tags, rss, ...


main :: IO ()
main = do
  setCurrentDirectory posts
  postsd <- listDirectory posts
  let reads = map (posts </>) . filter (not . isDotfile) $ postsd
  print reads
  mapM_ (readFile >=> print) reads
  post <- runPickle $ readPost "2014-08-17-karabiner-settings.md"
  print post
  where posts = "posts"
