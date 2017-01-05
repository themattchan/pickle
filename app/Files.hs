module Files where
import System.FilePath.Posix
import Text.Pandoc (Reader, readers)

isDotfile :: FilePath -> Bool
isDotfile = null . takeBaseName

data InputFormat = Markdown | OrgMode | Html -- | Lhs

instance Show InputFormat where
  show Markdown = "markdown"
  show OrgMode  = "org"
--  show Lhs      = "lhs"
  show Html     = "html"

getFileFormat :: FilePath -> Maybe InputFormat
getFileFormat fp = case tail (takeExtension fp) of
  "md"   -> Just Markdown
  "txt"  -> Just Markdown
  "org"  -> Just OrgMode
  "html" -> Just Html
  _      -> Nothing

getPandocReader :: InputFormat -> Maybe Reader
getPandocReader = flip lookup readers . show
