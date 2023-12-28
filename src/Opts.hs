{-# LANGUAGE StrictData #-}
module Opts(Opts(..), getOpts) where

import Data.Version(showVersion)
import Options.Applicative

import Paths_oplang(version)

data Opts =
  Opts
  { stackSize :: Word
  , tapeSize :: Word
  , noWarn :: Bool
  , noCC :: Bool
  , dumpAST :: Bool
  , dumpIR :: Bool
  , outPath :: Maybe FilePath
  , path :: FilePath
  }

getOpts :: IO Opts
getOpts = execParser optsParser
  where
    ver = "oplang v" <> showVersion version
    verOpt = infoOption ver (short 'v' <> long "version" <> help "Show version information")

    opts :: Parser Opts
    opts =
      Opts
      <$> option auto (long "stack-size" <> value 4096 <> metavar "SIZE" <> help "Size of the stack, in bytes (default 4096)")
      <*> option auto (long "tape-size" <> value 4096 <> metavar "SIZE" <> help "Size of the memory tape, in bytes (default 4096)")
      <*> switch (long "no-warn" <> help "Don't report warnings")
      <*> switch (long "no-cc" <> help "Output a C file without compiling it")
      <*> switch (long "dump-ast" <> help "Print the AST after parsing")
      <*> switch (long "dump-ir" <> help "Print the IR after optimization")
      <*> optional (strOption $ short 'o' <> long "out-path" <> metavar "PATH" <> help "Path of the resulting executable (or C file if --no-cc is passed)")
      <*> strArgument (metavar "PATH" <> help "The source file to compile")

    optsParser = info (helper <*> verOpt <*> opts) (fullDesc <> header ver)
