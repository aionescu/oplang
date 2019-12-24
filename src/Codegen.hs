{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Codegen(codegen) where

import Data.Char(ord)
import Numeric(showHex)

import qualified Data.HashMap.Strict as HashMap

import Data.Text(Text)

import Text.Builder(Builder)
import qualified Text.Builder as B

import Ast

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

allocTape :: CCode
allocTape = "char b[T],*t=b;memset(b,0,T);"

compileProto :: Name -> Body -> CCode
compileProto name _ = "void " <> cName name <> "();"

compileDef :: Name -> Body -> CCode
compileDef name body = "void " <> cName name <> "(){" <> allocTape <> compileOps body <> "}"

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
  OpCall c -> cName c <> "();"
  TailCall c -> cName c <> "();"

codegen :: Word -> Word -> Dict -> Text
codegen stackSize tapeSize d =
  B.run
  $ programPrologue stackSize tapeSize
  <> mconcat (HashMap.elems . HashMap.mapWithKey compileProto $ d)
  <> mconcat (HashMap.elems . HashMap.mapWithKey compileDef $ d)