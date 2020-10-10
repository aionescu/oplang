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

tryReadFile :: String -> IO (Either Text Text)
tryReadFile path = do
  exists <- doesFileExist path

  if exists
  then Right <$> T.readFile path
  else pure $ Left $ "Error: File '" <> pack path <> "' not found."

pipeline :: Word -> Either Text Text -> Either Text Dict
pipeline passes code =
  code
  >>= parse
  >>= check
  <&> optimize passes

runCompiler :: Opts -> IO ()
runCompiler opts@Opts{..} =
  tryReadFile optsPath
  <&> pipeline optsOptPasses
  >>= either T.putStrLn (C.compile opts)

main :: IO ()
main = runCompiler . changeOutPath =<< getOpts
