module Optimizer(optimize) where

import Data.List

import Ast

maxPasses = 64

canDoWithOffset :: Op -> Bool
canDoWithOffset (Move _) = False
canDoWithOffset (Loop _) = False
canDoWithOffset (Custom _) = False
canDoWithOffset (Tailcall _) = False
canDoWithOffset _ = True

optimizeOnce :: [Op] -> (Bool, [Op])
optimizeOnce = go False []
  where
    go :: Bool -> [Op] -> [Op] -> (Bool, [Op])
    go _ acc (Add 0 : ops) = go True acc ops
    go _ acc (Move 0 : ops) = go True acc ops
    go _ acc (Pop 0 : ops) = go True acc ops

    go _ acc (Loop [Add (-1)] : ops) = go True acc (Set 0 : ops)
    go _ acc (Loop [Add 1] : ops) = go True acc (Set 0 : ops)

    go _ acc (Set s : Add a : ops) = go True acc (Set (s + a) : ops)
    go _ acc (Add a : Add b : ops) = go True acc (Add (a + b) : ops)
    go _ acc (Move a : Move b : ops) = go True acc (Move (a + b) : ops)
    go _ acc (Pop a : Pop b : ops) = go True acc (Pop (a + b) : ops)

    go _ acc (Add _ : Read : ops) = go True acc (Read : ops)
    go _ acc (Add _ : Pop n : ops) = go True acc (Pop n : ops)
    go _ acc (Add _ : Set s : ops) = go True acc (Set s : ops)

    go _ acc (Set _ : Read : ops) = go True acc (Read : ops)
    go _ acc (Set _ : Pop n : ops) = go True acc (Pop n : ops)
    go _ acc (Set _ : Set s : ops) = go True acc (Set s : ops)

    go _ acc (Pop n : Push : ops) = go True acc (Pop (n - 1) : Peek : ops)
    go _ acc (Push : Pop n : ops) = go True acc (Pop (n - 1) : ops)

    go _ acc (Move m : op : Move n : ops) 
      | canDoWithOffset op && m == -n =
          case op of
            WithOffset o op' -> go True acc (WithOffset (m + o) op' : ops)
            _ -> go True acc (WithOffset m op : ops)

    go _ acc (Set 0 : Loop _ : ops) = go True acc (Set 0 : ops)
    go _ acc (Loop l : Loop _ : ops) = go True acc (Loop l : ops)
    go _ acc (Loop [Loop l] : ops) = go True acc (Loop l : ops)
    go _ acc (Loop l : ops) =
      let (changed, l') = go False [] l in
      go changed (Loop l' : acc) ops

    go _ acc [Custom c] = (True, reverse (Tailcall c : acc))
    go changed acc (op : ops) = go changed (op : acc) ops
    go changed acc [] = (changed, reverse acc)
  
optimizeN :: Int -> [Op] -> [Op]
optimizeN 0 ops = ops
optimizeN n ops =
  let (changed, ops') = optimizeOnce ops in
  if changed then
    optimizeN (n - 1) ops'
  else
    ops'

removeSet0 :: [Op] -> [Op]
removeSet0 (Set 0 : ops) = ops
removeSet0 ops = ops

optimizeOps :: [Op] -> [Op]
optimizeOps ops = removeSet0 $ optimizeN maxPasses (Set 0 : ops)

optimizeDef :: OpDef -> OpDef
optimizeDef (OpDef name ops) = OpDef name (optimizeOps ops)

optimize :: Program -> Program
optimize (Program defs mainOps) = Program (optimizeDef <$> defs) (optimizeOps mainOps)