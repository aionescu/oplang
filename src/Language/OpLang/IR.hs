module Language.OpLang.IR where

import Data.Int(Int8)
import Data.List(nub)
import Data.Bifunctor(first)
import Data.HashMap.Strict(HashMap)

data Op
  = Add Int8
  | Set Int8
  | Move Int
  | Pop Word
  | Push
  | Peek
  | Read
  | Write Word
  | WithOffset Int Op
  | Loop [Op]
  | OpCall Name
  | TailCall

type Name = Maybe Char
type Body = [Op]
type Def = (Name, Body)

type DefList = [Def]
type Dict = HashMap Name Body

calledOps :: Def -> [Name]
calledOps (name, ops) = nub $ go ops
  where
    go = \case
      OpCall c : rest -> c : go rest
      TailCall : rest -> name : go rest
      Loop l : rest -> go l ++ go rest
      _ : rest -> go rest
      [] -> []

type OpLang a = Either String a

err :: e -> Either e a
err = Left

toOpLang :: Show e => Either e a -> OpLang a
toOpLang = first show
