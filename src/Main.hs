module Main(main) where

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(asks)
import Data.Foldable(traverse_)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Exit(exitFailure)

import Control.Monad.Comp(Comp, runComp)
import Data.Opts(getOpts, Opts(..))
import Language.OpLang.Checker(check)
import Language.OpLang.Codegen(compile)
import Language.OpLang.Optimizer(optimize)
import Language.OpLang.Parser(parse)

getCode :: Comp Text
getCode = liftIO . T.readFile =<< asks optsPath

pipeline :: Comp ()
pipeline =
  getCode
  >>= parse
  >>= check
  >>= optimize
  >>= compile

main :: IO ()
main = do
  opts <- getOpts
  (warnings, result) <- runComp opts pipeline

  traverse_ T.putStrLn warnings
  maybe exitFailure pure result
