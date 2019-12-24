{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Function((&))
import Data.Functor((<&>))

import System.Directory(doesFileExist, removeFile)
import System.FilePath(dropExtension)
import System.Info(os)
import System.Process(system)

import Data.Text(Text, pack)
import qualified Data.Text.IO as T

import Text.Printf(printf)
import System.CPUTime(getCPUTime)

import Parser(parse)
import Checker(check)
import Optimizer(optimize)
import Codegen(codegen)
import Opts(Opts(..), getOpts)

pipeline :: Opts -> Text -> Either Text Text
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

-- https://wiki.haskell.org/Timing_computations
time :: IO a -> IO a
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime

  let diff = (fromIntegral (end - start)) / (10 ^ 12)
  printf "Done in %0.3fs.\n" (diff :: Double)

  pure v

main :: IO ()
main = {-time $-} do
  opts <- getOpts
  let path = optsPath opts

  exists <- doesFileExist path

  if not exists
  then T.putStrLn $ "Error: File '" <> pack path <> "' not found."
  else do
    code <- T.readFile path

    case pipeline opts code of
      Left e -> T.putStrLn e
      Right c -> compileC path c