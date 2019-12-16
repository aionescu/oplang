{-# LANGUAGE OverloadedStrings #-}

module Codegen(codegen) where

import Data.Char(ord)
import Numeric(showHex)

import Data.Text(Text)
import Data.Text as T

import Ast

type CCode = Text

showT :: Show a => a -> Text
showT = T.pack . show

customName :: Char -> Text
customName name = "o" <> T.pack (showHex (ord name) "")

programPrologue :: CCode
programPrologue = "#include<stdio.h>\n#include<string.h>\n#define S 65536\n#define T S\nchar sb[S],*s=sb;"

allocTape :: CCode
allocTape = "char b[T],*t=b;memset(b,0,T);"

mainPrologue :: CCode
mainPrologue = "int main(){" <> allocTape

compileProto :: Def -> CCode
compileProto (Def name _) = "void " <> customName name <> "();"

compileDef :: Def -> CCode
compileDef (Def name body) = "void " <> customName name <> "(){" <> allocTape <> compileOps body <> "}"

compileOps :: [Op] -> CCode
compileOps ops = mconcat $ compileOp "t" <$> ops

sign :: (Ord a, Num a) => a -> Text
sign n
  | n < 0 = "-"
  | otherwise = "+"

compileOp :: Text -> Op -> CCode
compileOp tape op = case op of
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
  OpCall c -> customName c <> "();"
  TailCall c -> customName c <> "();"

compileMain :: [Op] -> CCode
compileMain ops = mainPrologue <> compileOps ops <> "}"

codegen :: Program -> CCode
codegen (Program defs ops) = programPrologue <> mconcat (compileProto <$> defs) <> mconcat (compileDef <$> defs) <> compileMain ops