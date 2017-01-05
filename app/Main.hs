{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Except

import Data.Bifunctor
import Data.List
import Data.Maybe

import System.Directory
import System.FilePath.Posix

import Text.Pandoc
import Text.Pandoc.Error

import Text.Blaze

import qualified Config
import Files

type Pickle = ExceptT PickleError IO

runPickle :: Pickle a -> IO a
runPickle = fmap (bimap (error . show) id) . runExceptT

data PickleError = FileNotFound | PicklePandocError PandocError

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
  }

-- | Metadata for posts
data PostMeta = PostMeta
  { postMetaPandocMeta :: Meta
  , postMetaFilename   :: FilePath
  -- ^ full location in output
  , postMetaDstPath   :: FilePath
  -- ^ calculated from metadata (relative to generated site)
  }

postExtractMeta :: Post -> PostMeta
postExtractMeta p@Post{..} = PostMeta
  { postMetaPandocMeta = pandocMeta postContent
  , postMetaFilename   = postDstPath </> fromMaybe postName postOutName
  }

parseAsPandoc :: FilePath -> Pickle Pandoc
parseAsPandoc fp = ExceptT $ first PicklePandocError . reader def <$> readFile fp
  where
    StringReader reader = getPandocReader . getFileFormat $ fp

-- | relative filepaths
readPost :: FilePath -> Pickle Post
readPost fp = do
  isDir <- liftIO $ doesDirectoryExist fp
  if isDir then readPostFolder else readPostSingle
  where
    readPostFolder = do
      (contents, assets) <- filterContents <$> listDirectory fp
      if null contents
      then throwE FileNotFound
      else do
        let postName = fp </> head contents
        postContent <- parseAsPandoc postName
        let postSrcBundle = Just (fp, assets)
            postOutName = Nothing -- TODO
        return Post{..}

    readPostSingle = do
      postContent <- parseAsPandoc fp
      let postName = fp
          postOutName = Nothing -- TODO
          postSrcBundle = Nothing
      return Post{..}

    filterContents = partition ((`elem` contentFileNames) . takeBaseName)

renderPandoc :: Pandoc -> Html
renderPandoc = writeHtml def

-- | Write the post to disk, return metadata
writePost :: Post -> Pickle PostMeta
writePost Post{..} = undefined

buildIndex :: [PostMeta] -> Html
buildIndex = undefined

-- categories, tags, rss, ...


-- main :: IO ()
-- main = do
--   setCurrentDirectory posts
--   postsd <- listDirectory posts
--   let reads = map (posts </>) . filter (not . isDotfile) $ postsd
--   print reads
--   mapM_ (readFile >=> print) reads
