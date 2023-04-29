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

optsParser :: ParserInfo Opts
optsParser =
  info
    (infoOption ("oplang v" <> ver) (short 'v' <> long "version" <> help "Shows version information.")
      <*> helper
      <*> programOptions)
    (fullDesc
      <> progDesc "Compiles an OpLang source file to a native executable."
      <> header "oplang - The OpLang Compiler")
  where
    ver = showVersion version

    programOptions :: Parser Opts
    programOptions =
      Opts
      <$> option auto (long "stack-size" <> value 4096 <> metavar "SIZE" <> help "Size of the stack.")
      <*> option auto (long "tape-size" <> value 65536 <> metavar "SIZE" <> help "Size of the memory tape.")
      <*> switch (long "no-warn" <> help "Don't report warnings.")
      <*> switch (long "no-cc" <> help "Output a C file without compiling it.")
      <*> switch (long "dump-ast" <> help "Print the AST after parsing.")
      <*> switch (long "dump-ir" <> help "Print the IR after optimization.")
      <*> optional (strOption (short 'o' <> long "out-path" <> metavar "PATH" <> help "Path of the resulting executable (or C file if --no-cc is passed)."))
      <*> strArgument (metavar "PATH" <> help "The source file to compile.")

getOpts :: IO Opts
getOpts = execParser optsParser
