module Main(main) where

import Control.Category((>>>))
import Control.Monad((>=>))
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(asks)
import Data.Bifoldable(bitraverse_)
import Data.Foldable(traverse_)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Exit(exitFailure)

import Opts
import Comp
import Language.OpLang.Parse
import Language.OpLang.Validate
import Language.OpLang.Optimize
import Language.OpLang.Codegen

getCode :: Comp Text
getCode = liftIO . T.readFile =<< asks optsPath

pipeline :: Comp ()
pipeline =
  getCode >>= (parse >=> validate >=> optimize >>> compile)

main :: IO ()
main =
  getOpts
  >>= runComp pipeline
  >>= bitraverse_ (traverse_ T.putStrLn) (maybe exitFailure pure)
