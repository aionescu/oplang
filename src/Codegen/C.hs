{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}

module Codegen.C(compile) where

import Data.Char(ord)
import Numeric(showHex)

import qualified Data.HashMap.Strict as HashMap

import Data.Text(Text)
import qualified Data.Text.IO as T

import Text.Builder(Builder)
import qualified Text.Builder as B

import System.Directory(removeFile)
import System.FilePath(dropExtension)
import System.Process(system)

import AST(Op(..), Name, Body, Dict)
import Opts(Opts(..))

type CCode = Builder

showT :: Show a => a -> CCode
showT = B.string . show

cName :: Name -> CCode
cName Nothing = "main"
cName (Just name) = "o" <> B.string (showHex (ord name) "")

programPrologue :: Word -> Word -> CCode
programPrologue stackSize tapeSize =
  "#include<stdio.h>\n#include<string.h>\n#define S "
  <> showT stackSize
  <> "\n#define T "
  <> showT tapeSize
  <> "\nchar u[S],*s=u;"

compileProto :: Name -> Body -> CCode
compileProto name _ = "void " <> cName name <> "(char*t);"

compileDef :: Name -> Body -> CCode
compileDef name body = "void " <> cName name <> "(char*t){memset(t,0,T);" <> compileOps body <> "}"

compileMain :: Body -> CCode
compileMain body = "int main(){char b[T],*t=b;memset(b,0,T);" <> compileOps body <> "return 0;}" 

compileOps :: [Op] -> CCode
compileOps ops = mconcat $ compileOp "t" <$> ops

sign :: (Ord a, Num a) => a -> CCode
sign n
  | n < 0 = "-"
  | otherwise = "+"

compileOp :: CCode -> Op -> CCode
compileOp tape = \case
  Add n -> "*" <> tape <> sign n <> "=" <> showT (abs n) <> ";"
  Move n -> tape <> sign n <> "=" <> showT (abs n) <> ";"
  Set n -> "*" <> tape <> "=" <> showT n <> ";"
  Pop n -> "*" <> tape <> "=*(s-=" <> showT n <> ");"
  Push -> "*(s++)=*" <> tape <> ";"
  Peek -> "*" <> tape <> "=*(s-1);"
  WithOffset off op -> compileOp ("(" <> tape <> "+" <> showT off <> ")") op
  Loop ops -> "while(*t){" <> compileOps ops <> "}"
  Read -> "scanf(\"%c\"," <> tape <> ");"
  Write -> "printf(\"%c\",*" <> tape <> ");"
  OpCall c -> "{char b[T];" <> cName c <> "(b);}"
  TailCall c -> cName c <> "(t);"

codegen :: Word -> Word -> Dict -> Text
codegen stackSize tapeSize d =
  let d' = HashMap.delete Nothing d
  in
  B.run
  $ programPrologue stackSize tapeSize
  <> mconcat (HashMap.elems . HashMap.mapWithKey compileProto $ d')
  <> mconcat (HashMap.elems . HashMap.mapWithKey compileDef $ d')
  <> compileMain (d HashMap.! Nothing)

cFile :: String -> String
cFile file = dropExtension file <> ".c"

compile :: Opts -> Dict -> IO ()
compile Opts{..} d = do
  let cPath = cFile optsPath
  let code = codegen optsStackSize optsTapeSize d

  T.writeFile cPath code

  system ("cc -o " <> optsOutPath <> " " <> cPath)
  removeFile cPath