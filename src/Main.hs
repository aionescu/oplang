module Main(main) where

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(asks)
import Data.Foldable(traverse_)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Exit(exitFailure)

import Opts(getOpts, Opts(..))
import Comp(Comp, runComp)
import Language.OpLang.Parser(parse)
import Language.OpLang.Validate(validate)
import Language.OpLang.Optimizer(optimize)
import Language.OpLang.Codegen(compile)

getCode :: Comp Text
getCode = liftIO . T.readFile =<< asks optsPath

pipeline :: Comp ()
pipeline =
  getCode
  >>= parse
  >>= validate
  >>= optimize
  >>= compile

main :: IO ()
main = do
  opts <- getOpts
  (warnings, result) <- runComp opts pipeline

  traverse_ T.putStrLn warnings
  maybe exitFailure pure result
