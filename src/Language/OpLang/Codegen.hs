module Language.OpLang.Codegen(codegen) where

import Data.Char(ord)
import Data.Foldable(foldMap')
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text.Builder.Linear(Builder, fromDec, runBuilder)

import Language.OpLang.Syntax

type CCode = Builder

cName :: Id -> CCode
cName n = "o" <> fromDec (ord n)

programHeader :: Word -> Word -> CCode
programHeader stackSize tapeSize =
  "#include<stdio.h>\n#define T " <> fromDec tapeSize
  <> "\nchar q[" <> fromDec stackSize <> "],*s=q;"

forwardDecl :: Id -> CCode
forwardDecl name = "void " <> cName name <> "();"

codegenDef :: Id -> [Instr] -> CCode
codegenDef name body = "void " <> cName name <> "(){char u[T]={0},*t=u;" <> codegenOps body <> "}"

codegenMain :: [Instr] -> CCode
codegenMain body = "int main(){char u[T]={0},*t=u;" <> codegenOps body <> "return 0;}"

codegenOps :: [Instr] -> CCode
codegenOps = foldMap' codegenOp

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

codegenOp :: Instr -> CCode
codegenOp = \case
  Add n o -> tape o <> plusEq n <> fromDec (abs n) <> ";"
  Set n o -> tape o <> "=" <> fromDec n <> ";"
  Pop o -> tape o <> "=*(--s);"
  Push o -> "*(s++)=" <> tape o <> ";"
  Read o -> "scanf(\"%c\",&" <> tape o <> ");"
  Write o -> "printf(\"%c\"," <> tape o <> ");"
  Move n -> "t" <> plusEq n <> fromDec (abs n) <> ";"
  AddCell n o o' -> addCell n o o'
  Loop ops -> "while(*t){" <> codegenOps ops <> "}"
  Call c -> cName c <> "();"

codegen :: Word -> Word -> Program Instr -> Text
codegen stackSize tapeSize Program{..} =
  runBuilder
  $ programHeader stackSize tapeSize
  <> foldMap' forwardDecl (M.keys opDefs)
  <> M.foldMapWithKey codegenDef opDefs
  <> codegenMain topLevel
