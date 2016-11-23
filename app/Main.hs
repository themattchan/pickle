{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import Text.Pandoc

import qualified Config as Config
import Files

pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

pandocBody :: Pandoc -> [Block]
pandocBody (Pandoc _ b) = b

-- | Structure for posts
data Post = Post
  { postContent       :: Pandoc
  -- ^ the pandoc
  , postName          :: FilePath
  -- ^ source file name
  , postOutName       :: Maybe FilePath
  -- ^ out file name, if different
  , postSrcBundlePath :: Maybe FilePath
  -- ^ if it is in a folder, save the folder name for copying assets
  , postDstPath       :: FilePath
  -- ^ relative path
  }


-- | Metadata for posts
data PostMeta = PostMeta
  { postMetaPandocMeta :: Meta
  , postMetaFilename   :: FilePath
  -- ^ full location in output
  }

postExtractMeta :: Post -> PostMeta
postExtractMeta p@Post{..} = PostMeta
  { postMetaPandocMeta = pandocMeta postContent
  , postMetaFilename   = postDstPath </> fromMaybe postName postOutName
  }


-- | relative filepaths
readPost :: FilePath -> IO Post
readPost fp = do
  isDir <- doesDirectoryExist fp
  if isDir then
    let postSrcBundlePath = Just fp
    in undefined
  else do
    let postName = fp
    in undefined


writePost :: Post -> IO PostMeta
writePost = undefined


buildIndex :: [PostMeta] -> String
buildIndex = undefined

-- categories, tags, rss, ...


main :: IO ()
main = do
  setCurrentDirectory posts
  postsd <- listDirectory posts
  let reads = map (posts </>) . filter (not . isDotfile) $ postsd
  print reads
  mapM_ (readFile >=> print) reads
