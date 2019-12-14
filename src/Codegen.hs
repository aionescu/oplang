module Codegen(codegen) where

import Data.Char(ord)
import Numeric(showHex)

import Ast

type CCode = String

customName :: Char -> String
customName name = "o" ++ (showHex (ord name) "")

programPrologue :: CCode
programPrologue = "#include<stdio.h>\n#include<string.h>\n#define S 65536\n#define T S\nchar sb[S],*s=sb;"

allocTape :: CCode
allocTape = "char b[T],*t=b;memset(b,0,T);"

mainPrologue :: CCode
mainPrologue = "int main(){" ++ allocTape

compileProto :: Def -> CCode
compileProto (Def name _) = "void " ++ customName name ++ "();"

compileDef :: Def -> CCode
compileDef (Def name body) = "void " ++ customName name ++ "(){" ++ allocTape ++ compileOps body ++ "}"

compileOps :: [Op] -> CCode
compileOps ops = concat $ compileOp "t" <$> ops

sign :: (Ord a, Num a) => a -> String
sign n
  | n < 0 = "-"
  | otherwise = "+"

compileOp :: String -> Op -> CCode
compileOp tape (Add n) = "*" ++ tape ++ sign n ++ "=" ++ show (abs n) ++ ";"
compileOp tape (Move n) = tape ++ sign n ++ "=" ++ show (abs n) ++ ";"
compileOp tape (Set n) = "*" ++ tape ++ "=" ++ show n ++ ";"
compileOp tape (Pop n) = "*" ++ tape ++ "=*(s-=" ++ show n ++ ");"
compileOp tape Push = "*(s++)=*" ++ tape ++ ";"
compileOp tape Peek = "*" ++ tape ++ "=*(s-1);"
compileOp tape (WithOffset off op) = compileOp ("(" ++ tape ++ "+" ++ show off ++ ")") op
compileOp _ (Loop ops) = "while(*t){" ++ compileOps ops ++ "}"
compileOp tape Read = "scanf(\"%c\"," ++ tape ++ ");"
compileOp tape Write = "printf(\"%c\",*" ++ tape ++ ");"
compileOp _ (OpCall c) = customName c ++ "();"
compileOp _ (TailCall c) = customName c ++ "();"

compileMain :: [Op] -> CCode
compileMain ops = mainPrologue ++ compileOps ops ++ "}"

codegen :: Program -> CCode
codegen (Program defs ops) = programPrologue ++ concat (compileProto <$> defs) ++ concat (compileDef <$> defs) ++ compileMain ops