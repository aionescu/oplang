module Main where

import Data.Function((&))
import Data.Functor((<&>))
import System.Environment(getArgs)
import System.Directory(removeFile)
import System.FilePath(dropExtension)
import System.Info(os)
import System.Process(system)
import Text.Parsec(ParseError)

import Ast
import Parser
import Checker
import Optimizer
import Codegen

pipeline :: String -> Either String String
pipeline src =
  src
  & parse
  >>= check
  <&> optimize
  <&> compile

binaryFile :: String -> String
binaryFile file =
  let noExt = dropExtension file in
  case os of
    "mingw32" -> noExt ++ ".exe"
    _ -> noExt

cFile :: String -> String
cFile file = dropExtension file ++ ".c"

compileC :: String -> String -> IO ()
compileC file code = do
  let cPath = cFile file
  writeFile cPath code
  system ("cc -o " ++ binaryFile file ++ " " ++ cPath)
  removeFile cPath
  pure ()

main :: IO ()
main = do
  (path : _) <- getArgs
  code <- readFile path

  case pipeline code of
    Left e -> putStrLn e
    Right c -> compileC path c