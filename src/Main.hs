module Main(main) where

import Data.Functor((<&>))
import System.Directory(doesFileExist)
import Data.Text(Text, pack)
import qualified Data.Text.IO as T
import System.FilePath(dropExtension)
import System.Info(os)

import Language.OpLang.Parser
import Language.OpLang.Checker
import Language.OpLang.Optimizer
import Language.OpLang.Opts
import Language.OpLang.AST

import qualified Language.OpLang.Codegen.C as C

tryReadFile :: String -> IO (Either Text Text)
tryReadFile path = do
  exists <- doesFileExist path

  if exists
  then do
    f <- T.readFile path
    pure $ Right f
  else
    pure $ Left $ "Error: File '" <> pack path <> "' not found."

getOutPath :: String -> String
getOutPath file = dropExtension file ++ ext
  where
    ext = case os of
      "mingw32" -> ".exe"
      _ -> ".out"

changeOutPath :: Opts -> Opts
changeOutPath opts =
  case optsOutPath opts of
    "" -> opts { optsOutPath = getOutPath (optsPath opts) }
    _ -> opts

pipeline :: Word -> Either Text Text -> Either Text Dict
pipeline passes code =
  code
  >>= parse
  >>= check
  <&> optimize passes

runCompiler :: Opts -> IO ()
runCompiler opts@Opts{..} = do
  result <-
    tryReadFile optsPath
    <&> pipeline optsOptPasses

  case result of
    Left err -> T.putStrLn err
    Right defs -> C.compile opts defs

main :: IO ()
main = runCompiler . changeOutPath =<< getOpts
