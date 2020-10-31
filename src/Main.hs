module Main(main) where

import Data.Functor((<&>))
import System.Directory(doesFileExist)
import Data.Text(Text)
import qualified Data.Text.IO as T
import System.FilePath(dropExtension)
import System.Info(os)

import Language.OpLang.Syntax
import Language.OpLang.Parser(parse)
import Language.OpLang.Checker(check)
import Language.OpLang.Optimizer(optimize)
import Language.OpLang.Opts
import qualified Language.OpLang.Codegen.C as C

getOutPath :: String -> String
getOutPath file = dropExtension file ++ ext
  where
    ext = case os of
      "mingw32" -> ".exe"
      _ -> ".out"

changeOutPath :: Opts -> Opts
changeOutPath opts =
  case optsOutPath opts of
    "" -> opts { optsOutPath = getOutPath $ optsPath opts }
    _ -> opts

tryReadFile :: String -> IO (OpLang Text)
tryReadFile path = do
  exists <- doesFileExist path

  if exists
    then pure <$> T.readFile path
    else pure $ err $ "Error: File '" <> path <> "' not found."

pipeline :: Word -> OpLang Text -> OpLang Dict
pipeline passes code =
  code
    >>= parse
    >>= check
    <&> optimize passes

runCompiler :: Opts -> IO ()
runCompiler opts@Opts{..} =
  tryReadFile optsPath
    <&> pipeline optsOptPasses
    >>= either putStrLn (C.compile opts)

main :: IO ()
main = runCompiler . changeOutPath =<< getOpts
