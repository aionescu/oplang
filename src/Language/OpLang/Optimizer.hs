module Language.OpLang.Optimizer(optimize) where

import Data.List(union)
import qualified Data.Map.Strict as M

import Language.OpLang.IR

set0 :: Op
set0 = Set 0

canDoWithOffset :: Op -> Bool
canDoWithOffset (Move _) = False
canDoWithOffset (Loop _) = False
canDoWithOffset (OpCall _) = False
canDoWithOffset _ = True

optimizeOnce :: Body -> (Bool, Body)
optimizeOnce = go False []
  where
    go :: Bool -> Body -> Body -> (Bool, Body)
    go changed acc ops' = case ops' of
      Add 0 : ops -> go True acc ops
      Move 0 : ops -> go True acc ops
      Pop 0 : ops -> go True acc ops

      Loop [Add (-1)] : ops -> go True acc (set0 : ops)
      Loop [Add 1] : ops -> go True acc (set0 : ops)

      Set s : Add a : ops -> go True acc (Set (s + a) : ops)
      Add a : Add b : ops -> go True acc (Add (a + b) : ops)
      Move a : Move b : ops -> go True acc (Move (a + b) : ops)
      Pop a : Pop b : ops -> go True acc (Pop (a + b) : ops)
      Write a : Write b : ops -> go True acc (Write (a + b) : ops)

      Add _ : ops@(Read : _) -> go True acc ops
      Add _ : ops@(Pop _ : _) -> go True acc ops
      Add _ : ops@(Set _ : _) -> go True acc ops

      Set _ : ops@(Read : _) -> go True acc ops
      Set _ : ops@(Pop _ : _) -> go True acc ops
      Set _ : ops@(Set _ : _) -> go True acc ops

      Pop n : Push : ops -> go True acc (Pop (n - 1) : Peek : ops)
      Push : Pop n : ops -> go True acc (Pop (n - 1) : ops)

      Move m : op : Move n : ops
        | canDoWithOffset op && m == -n -> case op of
            WithOffset o op' -> go True acc (WithOffset (m + o) op' : ops)
            _ -> go True acc (WithOffset m op : ops)

      Set 0 : Loop _ : ops -> go True acc (set0 : ops)
      l@(Loop _) : Loop _ : ops -> go True acc (l : ops)
      Loop [l@(Loop _)] : ops -> go True acc (l : ops)
      Loop l : ops ->
        let (changed', l') = go False [] l
        in go changed' (Loop l' : acc) ops

      op : ops -> go changed (op : acc) ops
      [] -> (changed, reverse acc)

optimizeN :: Word -> Body -> Body
optimizeN 0 ops = ops
optimizeN n ops =
  if changed
  then optimizeN (n - 1) ops'
  else ops'

  where
    (changed, ops') = optimizeOnce ops

removeSet0 :: Body -> Body
removeSet0 (Set 0 : ops) = ops
removeSet0 ops = ops

optimizeOps :: Word -> Body -> Body
optimizeOps passes ops = removeSet0 $ optimizeN passes (set0 : ops)

callGraph :: Word -> Defs -> Defs -> [Name] -> Name -> Defs
callGraph passes defs acc toGo crr =
  let
    body = optimizeOps passes (defs M.! crr)
    called = filter (/= crr) $ calledOps body
    newAcc = M.insert crr body acc
  in
    case filter (not . (`M.member` acc)) (toGo `union` called) of
      [] -> newAcc
      (next : nexts) ->
        callGraph passes defs newAcc nexts next

optimize :: Word -> Defs -> Defs
optimize passes defs = callGraph passes defs M.empty [] Nothing
