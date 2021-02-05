module Main(main) where

import Control.Category((>>>))
import Data.Functor((<&>))
import Data.Text(Text)
import qualified Data.Text.IO as T
import System.Directory(doesFileExist)
import System.FilePath(dropExtension)
import System.Info(os)

import Language.OpLang.IR
import Language.OpLang.Parser(parse)
import Language.OpLang.Checker(check)
import Language.OpLang.Optimizer(optimize)
import qualified Language.OpLang.Codegen.C as C
import Opts

getOutPath :: String -> String
getOutPath file = dropExtension file ++ ext os
  where
    ext "mingw32" = ".exe"
    ext _ = ".out"

changeOutPath :: Opts -> Opts
changeOutPath opts =
  case optsOutPath opts of
    "" -> opts { optsOutPath = getOutPath $ optsPath opts }
    _ -> opts

tryReadFile :: String -> IO (Either String Text)
tryReadFile path = do
  exists <- doesFileExist path

  if exists
  then pure <$> T.readFile path
  else pure $ Left $ "Error: File '" <> path <> "' not found."

pipeline :: Word -> Either String Text -> Either String Defs
pipeline passes code =
  code
  >>= parse
  >>= check
  <&> optimize passes

runCompiler :: Opts -> IO ()
runCompiler opts@Opts{..} =
  tryReadFile optsPath
  >>=
    (pipeline optsOptPasses
    >>> either putStrLn (C.compile opts))

main :: IO ()
main = runCompiler . changeOutPath =<< getOpts
