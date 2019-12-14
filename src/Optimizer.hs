module Optimizer(optimize) where

import Ast

maxPasses = 64

canDoWithOffset :: Op -> Bool
canDoWithOffset (Move _) = False
canDoWithOffset (Loop _) = False
canDoWithOffset (OpCall _) = False
canDoWithOffset (TailCall _) = False
canDoWithOffset _ = True

optimizeOnce :: [Op] -> (Bool, [Op])
optimizeOnce = go False []
  where
    go :: Bool -> [Op] -> [Op] -> (Bool, [Op])
    go changed acc ops = case ops of
      Add 0 : ops -> go True acc ops
      Move 0 : ops -> go True acc ops
      Pop 0 : ops -> go True acc ops

      Loop [Add (-1)] : ops -> go True acc (set0 : ops)
      Loop [Add 1] : ops -> go True acc (set0 : ops)

      Set s : Add a : ops -> go True acc (Set (s + a) : ops)
      Add a : Add b : ops -> go True acc (Add (a + b) : ops)
      Move a : Move b : ops -> go True acc (Move (a + b) : ops)
      Pop a : Pop b : ops -> go True acc (Pop (a + b) : ops)

      Add _ : Read : ops -> go True acc (Read : ops)
      Add _ : Pop n : ops -> go True acc (Pop n : ops)
      Add _ : Set s : ops -> go True acc (Set s : ops)

      Set _ : Read : ops -> go True acc (Read : ops)
      Set _ : Pop n : ops -> go True acc (Pop n : ops)
      Set _ : Set s : ops -> go True acc (Set s : ops)

      Pop n : Push : ops -> go True acc (Pop (n - 1) : Peek : ops)
      Push : Pop n : ops -> go True acc (Pop (n - 1) : ops)

      Move m : op : Move n : ops 
        | canDoWithOffset op && m == -n -> case op of
            WithOffset o op' -> go True acc (WithOffset (m + o) op' : ops)
            _ -> go True acc (WithOffset m op : ops)

      Set 0 : Loop _ : ops -> go True acc (set0 : ops)
      Loop l : Loop _ : ops -> go True acc (Loop l : ops)
      Loop [Loop l] : ops -> go True acc (Loop l : ops)
      Loop l : ops ->
        let (changed, l') = go False [] l
        in go changed (Loop l' : acc) ops

      [OpCall c] -> go True acc [TailCall c]
      op : ops -> go changed (op : acc) ops
      [] -> (changed, reverse acc)
  
optimizeN :: Int -> [Op] -> [Op]
optimizeN 0 ops = ops
optimizeN n ops =
  if changed
  then optimizeN (n - 1) ops'
  else ops'

  where
    (changed, ops') = optimizeOnce ops

removeSet0 :: [Op] -> [Op]
removeSet0 (Set 0 : ops) = ops
removeSet0 ops = ops

optimizeOps :: [Op] -> [Op]
optimizeOps ops = removeSet0 $ optimizeN maxPasses (set0 : ops)

optimizeDef :: Def -> Def
optimizeDef (Def name ops) = Def name (optimizeOps ops)

optimize :: Program -> Program
optimize (Program defs mainOps) = Program (optimizeDef <$> defs) (optimizeOps mainOps)