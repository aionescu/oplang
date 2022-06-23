{-# LANGUAGE StrictData #-}

module Opts(Opts(..), getOpts) where

import Options.Applicative
import System.FilePath(dropExtension)
import System.Info(os)

data Opts =
  Opts
  { optsStackSize :: Word
  , optsTapeSize :: Word
  , optsKeepCFile :: Bool
  , optsCCPath :: FilePath
  , optsOutPath :: FilePath
  , optsPath :: FilePath
  }

optsParser :: ParserInfo Opts
optsParser =
  info
    (infoOption "oplang v0.3.0.0" (short 'v' <> long "version" <> help "Shows version information.")
      <*> helper
      <*> programOptions)
    (fullDesc
      <> progDesc "Compiles an OpLang source file to a native executable."
      <> header "oplang - The OpLang Compiler")

  where
    programOptions :: Parser Opts
    programOptions =
      Opts
      <$> option auto (long "stack-size" <> value 4096 <> metavar "SIZE" <> help "Size of the stack.")
      <*> option auto (long "tape-size" <> value 65536 <> metavar "SIZE" <> help "Size of the memory tape.")
      <*> switch (long "keep-c-file" <> help "Keep the resulting C file.")
      <*> strOption (long "cc-path" <> value "cc" <> metavar "PATH" <> help "Path of the C compiler to use.")
      <*> strOption (short 'o' <> long "out-path" <> value "" <> metavar "PATH" <> help "Path of the resulting executable.")
      <*> strArgument (metavar "PATH" <> help "The source file to compile.")

withExt :: FilePath -> FilePath
withExt path = dropExtension path <> ext os
  where
    ext "mingw32" = ".exe"
    ext _ = ".out"

setOutPath :: Opts -> Opts
setOutPath opts =
  case optsOutPath opts of
    "" -> opts { optsOutPath = withExt $ optsPath opts }
    _ -> opts

getOpts :: IO Opts
getOpts = setOutPath <$> execParser optsParser
