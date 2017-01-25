-- | Utility functions for working with pandoc
module Pickle.Pandoc
  ( pandocMeta
  , pandocBody
  , renderPandoc
  , pandocUnmeta

  -- * re-exports
  , lookupMeta
  ) where

import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Walk

import Text.Blaze.Html5

import Pickle.Types
import Pickle.Files

-- | Extractor methods for the Pandoc type defined in 'Text.Pandoc'
pandocMeta :: Pandoc -> Meta
pandocMeta (Pandoc m _) = m

pandocBody :: Pandoc -> [Block]
pandocBody (Pandoc _ b) = b

renderPandoc :: Pandoc -> Html
renderPandoc = writeHtml def

pandocUnmeta :: MetaValue -> String
pandocUnmeta = query go where
  go (Str s) = s
  go (Space) = " "
  go s       = error ("[pandocUnmeta] fill in pattern " ++ show s)


parseAsPandoc :: FilePath -> Pickle Pandoc
parseAsPandoc fp = parsedPd
  where
    reader = hoistEither
           $ note mempty
           $ getPandocReader =<< getFileFormat fp

    parsedPd = do
      file <- lift $ readFile fp
      StringReader r <- reader
      EitherT $ first PicklePandocError <$> r def file
