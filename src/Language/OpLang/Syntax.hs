{-# LANGUAGE StrictData #-}
module Language.OpLang.Syntax where

import Data.Int(Int8)
import Data.Map.Strict(Map)

type Name = Char
type Val = Int8
type Offset = Int

-- Surface-level AST, produced by the parser
data Op
  = Incr
  | Decr
  | MoveL
  | MoveR
  | Read'
  | Write'
  | Pop'
  | Push'
  | Loop' [Op]
  | Call' Name
  deriving stock Show

-- Internal IR, used for optimizations and codegen
data Instr
  = Add Offset Val
  | Set Offset Val
  | Read Offset
  | Write Offset
  | WriteKnown Val
  | Pop Offset
  | Push Offset
  | PushKnown Val
  | Move Offset
  | Loop [Instr]
  | Call Name
  | AddMul Offset Offset Val
  deriving stock Show

data Program op
  = Program
  { opDefs :: Map Name [op]
  , topLevel :: [op]
  }
  deriving stock Show
