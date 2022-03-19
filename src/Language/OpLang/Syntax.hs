module Language.OpLang.Syntax(Id, Op(..), Program(..), calledOps) where

import Data.Int(Int8)
import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.Set qualified as S

type Id = Char

data Op
  = Add !Int8
  | Set !Int8
  | Move !Int
  | Pop !Word
  | Push
  | Peek
  | Read
  | Write !Word
  | WithOffset !Int !Op
  | Loop ![Op]
  | Call !Id

data Program
  = Program
  { opDefs :: Map Id [Op]
  , topLevel :: [Op]
  }

calledOps :: [Op] -> Set Id
calledOps = foldMap \case
  Call op -> S.singleton op
  Loop ops -> calledOps ops
  _ -> S.empty
