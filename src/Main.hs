{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Function((&))
import Data.Functor((<&>))

import System.Environment(getArgs)
import System.Directory(removeFile)
import System.FilePath(dropExtension)
import System.Info(os)
import System.Process(system)

import Data.Text(Text)
import qualified Data.Text.IO as T

import Text.Parsec(ParseError)

import Parser(parse)
import Checker(check)
import Optimizer(optimize)
import Codegen(codegen)
import Opts(Opts(..), getOpts)

pipeline :: Opts -> Text -> Either String Text
pipeline Opts{..} src =
  src
  & parse
  >>= check
  <&> optimize optsOptPasses
  <&> codegen optsStackSize optsTapeSize

binaryFile :: String -> String
binaryFile file =
  case os of
    "mingw32" -> noExt <> ".exe"
    _ -> noExt
  where
    noExt = dropExtension file

cFile :: String -> String
cFile file = dropExtension file <> ".c"

compileC :: String -> Text -> IO ()
compileC file code = do
  let cPath = cFile file
  T.writeFile cPath code

  system ("cc -o " <> binaryFile file <> " " <> cPath)
  removeFile cPath
  
  pure ()

main :: IO ()
main = do
  opts <- getOpts
  let path = optsPath opts

  code <- T.readFile path

  case pipeline opts code of
    Left e -> putStrLn e
    Right c -> compileC path c