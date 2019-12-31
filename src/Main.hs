{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Functor((<&>))
import System.Directory(doesFileExist)
import Data.Text(Text, pack)
import qualified Data.Text.IO as T
import Text.Printf(printf)
import System.CPUTime(getCPUTime)
import System.FilePath(dropExtension)
import System.Info(os)

import Parser(parse)
import Checker(check)
import Optimizer(optimize)
import Opts(Opts(..), getOpts)
import Ast(Dict)

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

-- https://wiki.haskell.org/Timing_computations
time :: IO a -> IO a
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime

  let diff = (fromIntegral (end - start)) / (10 ^ 12)
  printf "Compiled in %0.3fs.\n" (diff :: Double)

  pure v

main :: IO ()
main = time $ do
  opts <- getOpts
  pipeline $ changeOutPath opts