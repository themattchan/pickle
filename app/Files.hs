module Files where

isDotfile :: FilePath -> Bool
isDotfile = isPrefixOf "."

data Formats = Markdown | OrgMode | Lhs | Html

instance Show Formats where
  show Markdown = "md"
  show OrgMode  = "org"
  show Lhs      = "lhs"
  show Html     = "html"
