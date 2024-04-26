module Language.OpLang.Codegen(codegen) where

import Data.Char(ord)
import Data.Foldable(foldMap')
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text.Builder.Linear(Builder, fromDec, runBuilder)

import Language.OpLang.Syntax

type CCode = Builder

cName :: Name -> CCode
cName name = "_" <> fromDec (ord name)

programHeader :: Word -> Word -> CCode
programHeader stackSize tapeSize =
  "#include<stdio.h>\n#define T " <> fromDec tapeSize
  <> "\nchar q[" <> fromDec stackSize <> "],*s=q;"

forwardDecl :: Name -> CCode
forwardDecl name = "void " <> cName name <> "();"

codegenDef :: Name -> [Instr] -> CCode
codegenDef name body = "void " <> cName name <> "(){char u[T]={0},*t=u;" <> codegenOps body <> "}"

codegenMain :: [Instr] -> CCode
codegenMain body = "int main(){char u[T]={0},*t=u;" <> codegenOps body <> "return 0;}"

codegenOps :: [Instr] -> CCode
codegenOps = foldMap' codegenOp

tape :: Offset -> CCode
tape 0 = "*t"
tape offset = "t[" <> fromDec offset <> "]"

plusEq :: (Ord a, Num a) => a -> CCode
plusEq val
  | val < 0 = "-="
  | otherwise = "+="

addMul :: Offset -> Offset -> Val -> CCode
addMul _ _ 0 = ""
addMul offset initialOffset val = tape offset <> plusEq val <> tape initialOffset <> times (abs val) <> ";"
  where
    times 1 = ""
    times n = "*" <> fromDec n

codegenOp :: Instr -> CCode
codegenOp = \case
  Add offset val -> tape offset <> plusEq val <> fromDec (abs val) <> ";"
  Set offset val -> tape offset <> "=" <> fromDec val <> ";"
  Pop offset -> tape offset <> "=*(--s);"
  Push offset -> "*(s++)=" <> tape offset <> ";"
  PushKnown val -> "*(s++)=" <> fromDec val <> ";"
  Read offset -> tape offset <> "=getchar();"
  Write offset -> "putchar(" <> tape offset <> ");"
  WriteKnown val -> "putchar(" <> fromDec val <> ");"
  Move offset -> "t" <> plusEq offset <> fromDec (abs offset) <> ";"
  AddMul offset initialOffset val -> addMul offset initialOffset val
  Loop ops -> "while(*t){" <> codegenOps ops <> "}"
  Call name -> cName name <> "();"

codegen :: Word -> Word -> Program Instr -> Text
codegen stackSize tapeSize Program{..} =
  runBuilder
  $ programHeader stackSize tapeSize
  <> foldMap' forwardDecl (M.keys opDefs)
  <> M.foldMapWithKey codegenDef opDefs
  <> codegenMain topLevel
