module Ast where

import Data.Int(Int8)
import Data.Word(Word8)

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
  | OpCall Char
  | TailCall Char
  deriving (Show, Eq)

incr = Add 1
decr = Add (-1)
movl = Move (-1)
movr = Move 1
pop = Pop 1
set0 = Set 0

data Def =
  Def { defName :: Char, defBody :: [Op] }
  deriving Show

data Program =
  Program { programDefs :: [Def], programTopLevel :: [Op] }
  deriving Show