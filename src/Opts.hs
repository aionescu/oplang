module Opts(Opts(..), getOpts) where

import Options.Applicative

data Opts = Opts {
  optsOptPasses :: Word,
  optsStackSize :: Word,
  optsTapeSize :: Word,
  optsOutPath :: String,
  optsPath :: String
} deriving Show

defaultOptPasses :: Word
defaultOptPasses = 64

defaultStackSize :: Word
defaultStackSize = 4096

defaultTapeSize :: Word
defaultTapeSize = defaultStackSize

defaultOutPath :: String
defaultOutPath = ""

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> programOptions)
    (fullDesc
      <> progDesc "Compiles an OpLang source file to a native executable."
      <> header "oplangc - The OpLang Compiler")

  where
    programOptions :: Parser Opts
    programOptions =
      Opts
        <$> option auto (short 'O' <> long "opt-passes" <> metavar "PASSES" <> value defaultOptPasses <> help "Specify the number of optimization passes to perform.")
        <*> option auto (short 'S' <> long "stack-size" <> metavar "STACK" <> value defaultStackSize <> help "Specify the size of the stack.")
        <*> option auto (short 'T' <> long "tape-size" <> metavar "TAPE" <> value defaultTapeSize <> help "Specify the size of the memory tape.")
        <*> strOption (short 'o' <> long "out-path" <> metavar "OUT_PATH" <> value defaultOutPath <> help "Specify the path of the resulting executable.")
        <*> strArgument (metavar "PATH" <> help "The source file to compile.")

getOpts :: IO Opts
getOpts = execParser optsParser