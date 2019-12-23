{-# LANGUAGE LambdaCase #-}

module Ast where

import Data.Int(Int8)
import Data.List(nub)
import Data.Word(Word8)

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap

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

type Body = [Op]
type Def = (Maybe Char, Body)
type Name = Maybe Char
type Dict = HashMap Name Body

incr = Add 1
decr = Add (-1)
movl = Move (-1)
movr = Move 1
pop = Pop 1
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