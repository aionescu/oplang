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

toName :: Char -> Name
toName = Just

fromName :: Name -> String
fromName Nothing = "toplevel"
fromName (Just n) = [n]

incr = Add 1
decr = Add (-1)
movl = Move (-1)
movr = Move 1
pop = Pop 1
set0 = Set 0

calledOps :: [Op] -> [Name]
calledOps = nub . \case
  OpCall c : rest -> c : calledOps rest
  TailCall c : rest -> c : calledOps rest
  Loop l : rest -> calledOps l ++ calledOps rest
  _ : rest -> calledOps rest
  [] -> []