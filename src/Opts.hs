module Opts(Opts(..), getOpts) where

import Options.Applicative
import System.FilePath(dropExtension)
import System.Info(os)

data Opts =
  Opts
  { optsOptPasses :: Word
  , optsStackSize :: Word
  , optsTapeSize :: Word
  , optsKeepCFile :: Bool
  , optsCCPath :: FilePath
  , optsOutPath :: FilePath
  , optsPath :: FilePath
  }

optsParser :: ParserInfo Opts
optsParser =
  info
    (infoOption "oplang v0.1.0.0" (short 'v' <> long "version" <> help "Shows version information.")
      <*> helper
      <*> programOptions)
    (fullDesc
      <> progDesc "Compiles an OpLang source file to a native executable."
      <> header "oplang - The OpLang Compiler")

  where
    programOptions :: Parser Opts
    programOptions =
      Opts
        <$> option auto (short 'O' <> long "opt-passes" <> metavar "PASSES" <> value 64 <> help "Specify the number of optimization passes to perform.")
        <*> option auto (short 'S' <> long "stack-size" <> metavar "STACK" <> value 4096 <> help "Specify the size of the stack.")
        <*> option auto (short 'T' <> long "tape-size" <> metavar "TAPE" <> value 65536 <> help "Specify the size of the memory tape.")
        <*> switch (short 'K' <> long "keep-c-file" <> help "Specifiy whether to keep the resulting C file.")
        <*> strOption (short 'C' <> long "cc-path" <> metavar "CC_PATH" <> value "cc" <> help "Specify the path of the C compiler to use.")
        <*> strOption (short 'o' <> long "out-path" <> metavar "OUT_PATH" <> value "" <> help "Specify the path of the resulting executable.")
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
