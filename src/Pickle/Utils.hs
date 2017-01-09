module Pickle.Utils where

import Data.List
import Data.Maybe
import Data.Either

note e Nothing  = Left e
note _ (Just x) = Right x

hush (Left _)  = Nothing
hush (Right x) = Just x
