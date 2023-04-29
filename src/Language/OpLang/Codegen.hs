module Language.OpLang.Codegen(compile) where

import Control.Monad.Reader(ask)
import Control.Monad.Trans(lift)
import Data.Char(ord)
import Data.Foldable(foldMap')
import Data.Map.Strict qualified as M
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Text.Builder.Linear(Builder, fromDec, runBuilder)
import Data.Text.IO qualified as T
import System.Directory(createDirectoryIfMissing, removeFile)
import System.Environment(lookupEnv)
import System.FilePath(dropExtension, takeDirectory)
import System.Info(os)
import System.Process(callProcess)

import Control.Monad.Comp(CompT)
import Language.OpLang.Syntax
import Opts(Opts(..))

type CCode = Builder

cName :: Id -> CCode
cName n = "o" <> fromDec (ord n)

programHeader :: Word -> Word -> CCode
programHeader stackSize tapeSize =
  "#include<stdio.h>\n#define T " <> fromDec tapeSize
  <> "\nchar q[" <> fromDec stackSize <> "],*s=q;"

forwardDecl :: Id -> CCode
forwardDecl name = "void " <> cName name <> "();"

compileDef :: Id -> [Instr] -> CCode
compileDef name body = "void " <> cName name <> "(){char u[T]={0},*t=u;" <> compileOps body <> "}"

compileMain :: [Instr] -> CCode
compileMain body = "int main(){char u[T]={0},*t=u;" <> compileOps body <> "return 0;}"

compileOps :: [Instr] -> CCode
compileOps = foldMap' compileOp

tape :: Offset -> CCode
tape 0 = "*t"
tape off = "t[" <> fromDec off <> "]"

plusEq :: (Ord a, Num a) => a -> CCode
plusEq n
  | n < 0 = "-="
  | otherwise = "+="

addCell :: Val -> Offset -> Offset -> CCode
addCell v o o' = tape o <> plusEq v <> tape o' <> times (abs v) <> ";" <> tape o' <> "=0;"
  where
    times 1 = ""
    times n = "*" <> fromDec n

compileOp :: Instr -> CCode
compileOp = \case
  Add n o -> tape o <> plusEq n <> fromDec (abs n) <> ";"
  Set n o -> tape o <> "=" <> fromDec n <> ";"
  Pop o -> tape o <> "=*(--s);"
  Push o -> "*(s++)=" <> tape o <> ";"
  Read o -> "scanf(\"%c\",&" <> tape o <> ");"
  Write o -> "printf(\"%c\"," <> tape o <> ");"
  Move n -> "t" <> plusEq n <> fromDec (abs n) <> ";"
  AddCell n o o' -> addCell n o o'
  Loop ops -> "while(*t){" <> compileOps ops <> "}"
  Call c -> cName c <> "();"

codegen :: Word -> Word -> Program Instr -> Text
codegen stackSize tapeSize Program{..} =
  runBuilder
  $ programHeader stackSize tapeSize
  <> foldMap' forwardDecl (M.keys opDefs)
  <> M.foldMapWithKey compileDef opDefs
  <> compileMain topLevel

exePath :: FilePath -> FilePath
exePath path = dropExtension path <> ext os
  where
    ext "mingw32" = ".exe"
    ext _ = ".out"

ccPath :: IO FilePath
ccPath = fromMaybe "cc" <$> lookupEnv "CC"

compile :: Program Instr -> CompT IO ()
compile p = do
  Opts{..} <- ask
  let cFile = dropExtension path <> ".c"
  let cCode = codegen stackSize tapeSize p

  lift
    if noCC then do
      let outFile = fromMaybe cFile outPath
      createDirectoryIfMissing True $ takeDirectory outFile

      T.writeFile outFile cCode
    else do
      T.writeFile cFile cCode

      let outFile = fromMaybe (exePath path) outPath
      createDirectoryIfMissing True $ takeDirectory outFile

      cc <- ccPath
      callProcess cc ["-o", outFile, cFile]
      removeFile cFile
