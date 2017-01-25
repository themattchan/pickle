{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | this is a dump of the whole program for now...
module Pickle.Everything where

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

import qualified Text.Blaze.Html4.Strict as B
import qualified Text.Blaze.Html.Renderer.Text as B (renderHtml)

import Pickle.Types
import qualified Pickle.Config as Config
import Pickle.Files
import Pickle.Utils
import Pickle.Template
import Pickle.Pandoc

-- | Names of files containing the text of an article in a folder
-- TODO move to config, add a reader
contentFileNames :: [String]
contentFileNames = [ "index" ]

-- postExtractMeta :: Post -> PostMeta
-- postExtractMeta p@Post{..} = PostMeta
--   { postMetaPandocMeta = pandocMeta postContent
--   , postMetaFilename   = postDstPath </> fromMaybe postName postOutName
--   }


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
      pandocMeta
      let postName = fp
          postOutName = Nothing -- TODO
          postSrcBundle = Nothing
      return Post{..}

    filterContents = partition ((`elem` contentFileNames) . takeBaseName)

-- | Write the post to disk, return metadata
writePost :: Post -> Pickle PostMeta
writePost Post{..} = undefined

buildIndex :: [PostMeta] -> B.Html
buildIndex = undefined

-- categories, tags, rss, ...

main :: IO ()
main = do
  currd <- getCurrentDirectory
  let posts = currd </> "posts"
  setCurrentDirectory posts
  postsd <- listDirectory posts
  let reads = map (posts </>) . filter (not . isDotfile) $ postsd
--  print reads
--  mapM_ (readFile >=> print) reads
  post <- runPickle $ readPost "2014-08-17-karabiner-settings.md"
  writeFile "foo.html" $ T.unpack $ B.renderHtml $ assemble $ myBlogPost post
