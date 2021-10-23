module Main(main) where

import Control.Category((>>>))
import Control.Monad((>=>))
import Data.Function((&))
import Data.Functor((<&>))
import Data.Text.IO qualified as T

import Language.OpLang.Parser(parse)
import Language.OpLang.Checker(check)
import Language.OpLang.Optimizer(optimize)
import Language.OpLang.Codegen.C qualified as C
import Opts

runCompiler :: Opts -> IO ()
runCompiler opts@Opts{..} =
  optsPath &
    ( T.readFile
      >=> (parse >=> check)
      >>> (<&> optimize optsOptPasses)
      >>> either T.putStrLn (C.compile opts)
    )

main :: IO ()
main = getOpts >>= runCompiler
