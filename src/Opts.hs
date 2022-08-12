{-# LANGUAGE StrictData #-}

module Opts(Opts(..), getOpts) where

import Options.Applicative

data Opts =
  Opts
  { optsStackSize :: Word
  , optsTapeSize :: Word
  , optsKeepCFile :: Bool
  , optsCCPath :: FilePath
  , optsOutPath :: Maybe FilePath
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
      <$> option auto (short 'S' <> long "stack-size" <> value 4096 <> metavar "SIZE" <> help "Size of the stack.")
      <*> option auto (short 'T' <> long "tape-size" <> value 65536 <> metavar "SIZE" <> help "Size of the memory tape.")
      <*> switch (short 'K' <> long "keep-c-file" <> help "Keep the resulting C file.")
      <*> strOption (short 'C' <> long "cc-path" <> value "cc" <> metavar "PATH" <> help "Path of the C compiler to use.")
      <*> optional (strOption (short 'o' <> long "out-path" <> metavar "PATH" <> help "Path of the resulting executable."))
      <*> strArgument (metavar "PATH" <> help "The source file to compile.")

getOpts :: IO Opts
getOpts = execParser optsParser
