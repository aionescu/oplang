{-# LANGUAGE StrictData #-}

module Language.OpLang.IR where

import Data.Int(Int8)
import Data.Map.Strict(Map)

type Id = Char
type Val = Int8
type Offset = Int

-- "Surface-level" AST, produced by the parser
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
  | Call' Id

-- Internal AST, used for optimizations and codegen
data Instr
  = Add Val Offset
  | Set Val Offset
  | Read Offset
  | Write Offset
  | Pop Offset
  | Push Offset
  | Move Offset
  | Loop [Instr]
  | Call Id
  | AddCell Val Offset Offset

data Program op
  = Program
  { opDefs :: Map Id [op]
  , topLevel :: [op]
  }
