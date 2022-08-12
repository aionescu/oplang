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

import Language.OpLang.Codegen(compile)
import Language.OpLang.CompT(CompT(..))
import Language.OpLang.Optimize(optimize)
import Language.OpLang.Parse(parse)
import Language.OpLang.Validate(validate)
import Opts(Opts(..), getOpts)

getCode :: CompT IO Text
getCode = lift . T.readFile =<< asks optsPath

pipeline :: CompT IO ()
pipeline =
  getCode >>= (parse >=> validate >=> optimize >>> compile)

main :: IO ()
main =
  getOpts
  >>= runCompT pipeline
  >>= bitraverse_ (maybe exitFailure pure) (traverse_ T.putStrLn)
