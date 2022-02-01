module Main(main) where

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(asks)
import Data.Foldable(traverse_)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Exit(exitFailure)

import Language.OpLang.Checker(check)
import Language.OpLang.Codegen.C qualified as C
import Language.OpLang.Comp( Comp, runComp)
import Language.OpLang.Optimizer(optimize)
import Language.OpLang.Parser(parse)
import Opts(getOpts, Opts(..))

getCode :: Comp Text
getCode = liftIO . T.readFile =<< asks optsPath

pipeline :: Comp ()
pipeline =
  getCode
  >>= parse
  >>= check
  >>= optimize
  >>= C.compile

main :: IO ()
main = do
  opts <- getOpts
  (warnings, result) <- runComp opts pipeline

  traverse_ T.putStrLn warnings
  maybe exitFailure pure result
