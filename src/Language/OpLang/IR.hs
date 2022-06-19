{-# LANGUAGE StrictData #-}

module Language.OpLang.IR(Id, NoOff, Off, Op(..), Program(..), calledOps) where

import Data.Int(Int8)
import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.Set qualified as S

type Id = Char

type NoOff = ()
type Off = Int

data Op o
  = Add o Int8
  | Set o Int8
  | Pop o Word
  | Push o
  | Peek o
  | Read o
  | Write o Word
  | Move Off
  | AddTimes Off Int8
  | Loop [Op o]
  | Call Id

data Program o
  = Program
  { opDefs :: Map Id [Op o]
  , topLevel :: [Op o]
  }

calledOps :: [Op o] -> Set Id
calledOps = foldMap \case
  Call op -> S.singleton op
  Loop ops -> calledOps ops
  _ -> S.empty
