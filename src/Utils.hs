module Utils where

import Data.Text(Text)
import Data.Text qualified as T

showT :: Show a => a -> Text
showT = T.pack . show
