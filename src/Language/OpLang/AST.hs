module Language.OpLang.AST where

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

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right c) = Right c
mapLeft f (Left a) = Left $ f a

toOpLang :: Show e => Either e a -> OpLang a
toOpLang = mapLeft show
