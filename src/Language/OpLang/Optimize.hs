module Language.OpLang.Optimize(optimize) where

import Control.Monad.ST(ST, runST)
import Data.Foldable(fold)
import Data.Functor(($>))
import Data.STRef(STRef, readSTRef, newSTRef, writeSTRef, modifySTRef')

import Language.OpLang.IR

toInstr :: Op -> Instr
toInstr = \case
  Incr -> Add 1 0
  Decr -> Add -1 0
  MoveL -> Move -1
  MoveR -> Move 1
  Read' -> Read 0
  Write' -> Write 0
  Pop' -> Pop 0
  Push' -> Push 0
  Loop' ops -> Loop $ toInstr <$> ops
  Call' c -> Call c

peephole :: [Instr] -> [Instr]
peephole = go []
  where
    go :: [Instr] -> [Instr] -> [Instr]
    go acc = \case
      Add 0 _ : ops -> go acc ops
      Move 0 : ops -> go acc ops

      Loop [Add -1 _] : ops -> go acc (Set 0 0 : ops)
      Loop [Add 1 _] : ops -> go acc (Set 0 0 : ops)

      Set s _ : Add a _ : ops -> go acc (Set (s + a) 0 : ops)
      Add a _ : Add b _ : ops -> go acc (Add (a + b) 0 : ops)
      Move a : Move b : ops -> go acc (Move (a + b) : ops)

      Add{} : ops@(Read{} : _) -> go acc ops
      Add{} : ops@(Pop{} : _) -> go acc ops
      Add{} : ops@(Set{} : _) -> go acc ops

      Set{} : ops@(Read{} : _) -> go acc ops
      Set{} : ops@(Pop{} : _) -> go acc ops
      Set{} : ops@(Set{} : _) -> go acc ops

      Set 0 _ : Loop _ : ops -> go acc (Set 0 0 : ops)
      l@(Loop _) : Loop _ : ops -> go acc (l : ops)
      Loop [l@(Loop _)] : ops -> go acc (l : ops)
      Loop l : ops -> go (Loop (peephole l) : acc) ops

      op : ops -> go (op : acc) ops
      [] -> reverse acc

removeSet0 :: [Instr] -> [Instr]
removeSet0 (Set 0 _ : ops) = ops
removeSet0 ops = ops

sync :: STRef s Offset -> ST s [Instr]
sync r = readSTRef r >>= \case
  0 -> pure []
  n -> writeSTRef r 0 $> [Move n]

withOffsetLoop :: STRef s Offset -> [Instr] -> ST s Instr
withOffsetLoop r ops = (\a b -> mkLoop $ fold a <> b) <$> traverse (withOffset r) ops <*> sync r
  where
    mkLoop [Add n o, Add -1 0] = AddCell n o
    mkLoop [Add -1 0, Add n o] = AddCell n o
    mkLoop l = Loop l

withOffset :: STRef s Offset -> Instr -> ST s [Instr]
withOffset r op = readSTRef r >>= \off ->
  case op of
    Add n _ -> pure [Add n off]
    Set n _ -> pure [Set n off]
    Pop _ -> pure [Pop off]
    Push _ -> pure [Push off]
    Read _ -> pure [Read off]
    Write _ -> pure [Write off]
    Move n -> modifySTRef' r (+ n) $> []
    Loop ops -> (\a b -> a <> [b]) <$> sync r <*> withOffsetLoop r ops
    Call c -> pure [Call c]
    AddCell{} -> error "withOffset: Unreacahble"

withOffsetOps :: [Instr] -> [Instr]
withOffsetOps ops = fold $ runST do
  r <- newSTRef 0
  traverse (withOffset r) ops

optimizeOps :: [Op] -> [Instr]
optimizeOps ops = withOffsetOps $ removeSet0 $ peephole $ Set 0 0 : (toInstr <$> ops)

optimize :: Program Op -> Program Instr
optimize Program{..} =
  Program
  { opDefs = optimizeOps  <$> opDefs
  , topLevel = optimizeOps topLevel
  }
