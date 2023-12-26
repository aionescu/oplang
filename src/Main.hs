module Main(main) where

import Control.Monad(when)
import Control.Monad.Chronicle(ChronicleT(..))
import Control.Monad.Trans(lift)
import Data.Bifoldable(bitraverse_)
import Data.Foldable(traverse_)
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Directory(createDirectoryIfMissing, removeFile)
import System.Environment(lookupEnv)
import System.Exit(exitFailure, exitSuccess)
import System.FilePath(dropExtension, takeDirectory)
import System.Info(os)
import System.Process(callProcess)

import Language.OpLang.Codegen(codegen)
import Language.OpLang.Optimizer(optimize)
import Language.OpLang.Parser(parse)
import Language.OpLang.Validation(validate)
import Opts(Opts(..), getOpts)

exePath :: FilePath -> FilePath
exePath path = dropExtension path <> ext os
  where
    ext "mingw32" = ".exe"
    ext _ = ".out"

runCompiler :: Opts -> Text -> ChronicleT [Text] IO ()
runCompiler opts code = do
  ast <- parse opts.path code
  when (dumpAST opts) do
    lift $ putStrLn $ "AST:\n" <> show ast <> "\n"

  ast' <- validate opts.noWarn ast

  let ir = optimize ast'
  when (dumpIR opts) do
    lift $ putStrLn $ "IR:\n" <> show ir <> "\n"

  let
    cCode = codegen opts.stackSize opts.tapeSize ir
    cFile = dropExtension opts.path <> ".c"

  lift
    if opts.noCC then do
      let outFile = fromMaybe cFile opts.outPath
      createDirectoryIfMissing True $ takeDirectory outFile
      T.writeFile outFile cCode
    else do
      T.writeFile cFile cCode

      let outFile = fromMaybe (exePath opts.path) opts.outPath
      createDirectoryIfMissing True $ takeDirectory outFile

      cc <- fromMaybe "cc" <$> lookupEnv "CC"
      callProcess cc ["-o", outFile, cFile]
      removeFile cFile

main :: IO ()
main = do
  opts <- getOpts
  code <- T.readFile opts.path

  runChronicleT (runCompiler opts code)
    >>= bitraverse_ (traverse_ T.putStrLn) (\_ -> exitSuccess)

  exitFailure
