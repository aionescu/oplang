module Main(main) where

import Data.Functor((<&>))
import System.Directory(doesFileExist)
import Data.Text(Text, pack)
import qualified Data.Text.IO as T
import System.FilePath(dropExtension)
import System.Info(os)

import Parser(parse)
import Checker(check)
import Optimizer(optimize)
import Opts(Opts(..), getOpts)
import AST(Dict)

import qualified Codegen.C as C
-- import qualified Codegen.LLVM as LLVM

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

pipelinePure :: Word -> Either Text Text -> Either Text Dict
pipelinePure passes code =
  code
  >>= parse
  >>= check
  <&> optimize passes

pipeline :: Opts -> IO ()
pipeline opts@Opts{..} = do
  either <-
    tryReadFile optsPath
    <&> pipelinePure optsOptPasses

  case either of
    Left err -> T.putStrLn err
    Right code -> C.compile opts code

main :: IO ()
main = do
  opts <- getOpts
  pipeline $ changeOutPath opts