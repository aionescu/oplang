module Ast where

import Data.Int
import Data.List
import Data.Word

data Op
  = Add Int8
  | Set Int8
  | Move Int
  | Pop Word
  | Push
  | Peek
  | Read
  | Write
  | WithOffset Int Op
  | Loop [Op]
  | Custom Char
  | Tailcall Char
  deriving Show

data OpDef =
  OpDef { opDefName :: Char, opDefBody :: [Op] }
  deriving Show

data Program =
  Program { programDefs :: [OpDef], programTopLevel :: [Op] }
  deriving Show

calledOps :: [Op] -> [Char]
calledOps = nub . go
  where
    go (Custom c : rest) = c : calledOps rest
    go (Tailcall c : rest) = c : calledOps rest
    go (Loop l : rest) = calledOps l ++ calledOps rest
    go (_ : rest) = calledOps rest
    go [] = []