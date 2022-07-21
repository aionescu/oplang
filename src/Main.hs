module Main(main) where

import Control.Category((>>>))
import Control.Monad((>=>))
import Control.Monad.Reader(asks)
import Control.Monad.Trans(lift)
import Data.Bifoldable(bitraverse_)
import Data.Foldable(traverse_)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Exit(exitFailure)

import Language.OpLang.Codegen
import Language.OpLang.CompT
import Language.OpLang.Optimize
import Language.OpLang.Parse
import Language.OpLang.Validate
import Opts

getCode :: CompT IO Text
getCode = lift . T.readFile =<< asks optsPath

pipeline :: CompT IO ()
pipeline =
  getCode >>= (parse >=> validate >=> optimize >>> compile)

main :: IO ()
main =
  getOpts
  >>= runCompT pipeline
  >>= bitraverse_ (traverse_ T.putStrLn) (maybe exitFailure pure)
