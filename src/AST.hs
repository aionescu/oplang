module AST(Op(..), Body, Def, Name, Dict, DefList, incr, decr, movl, movr, pop, set0, calledOps) where

import Data.Int(Int8)
import Data.List(nub)

import Data.HashMap.Strict(HashMap)

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
  | OpCall Name
  | TailCall Name

type Name = Maybe Char
type Body = [Op]
type Def = (Name, Body)

type DefList = [Def]
type Dict = HashMap Name Body

incr :: Op
incr = Add 1

decr :: Op
decr = Add (-1)

movl :: Op
movl = Move (-1)

movr :: Op
movr = Move 1

pop :: Op
pop = Pop 1

set0 :: Op
set0 = Set 0

calledOps :: [Op] -> [Name]
calledOps = nub . go
  where
    go = \case
      OpCall c : rest -> c : go rest
      TailCall c : rest -> c : go rest
      Loop l : rest -> go l ++ go rest
      _ : rest -> go rest
      [] -> []