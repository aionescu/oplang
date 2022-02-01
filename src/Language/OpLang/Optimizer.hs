module Language.OpLang.Optimizer(optimize) where

import Control.Category((>>>))
import Control.Monad.Reader(asks)

import Language.OpLang.Comp(Comp)
import Language.OpLang.Syntax(Program(..), Op(..))
import Opts(Opts(..))

canDoWithOffset :: Op -> Bool
canDoWithOffset (Move _) = False
canDoWithOffset (Loop _) = False
canDoWithOffset (Call _) = False
canDoWithOffset _ = True

optimizeOnce :: [Op] -> (Bool, [Op])
optimizeOnce = go False []
  where
    go :: Bool -> [Op] -> [Op] -> (Bool, [Op])
    go changed acc ops' =
      case ops' of
        Add 0 : ops -> go True acc ops
        Move 0 : ops -> go True acc ops
        Pop 0 : ops -> go True acc ops

        Loop [Add (-1)] : ops -> go True acc (Set 0 : ops)
        Loop [Add 1] : ops -> go True acc (Set 0 : ops)

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

        Set 0 : Loop _ : ops -> go True acc (Set 0 : ops)
        l@(Loop _) : Loop _ : ops -> go True acc (l : ops)
        Loop [l@(Loop _)] : ops -> go True acc (l : ops)
        Loop l : ops ->
          let (changed', l') = go False [] l
          in go changed' (Loop l' : acc) ops

        op : ops -> go changed (op : acc) ops
        [] -> (changed, reverse acc)

optimizeN :: Word -> [Op] -> [Op]
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

optimizeOps :: Word -> [Op] -> [Op]
optimizeOps passes ops = removeSet0 $ optimizeN passes (Set 0 : ops)

optimize :: Program -> Comp Program
optimize Program{..} =
  asks $ optsOptPasses >>> \passes ->
    Program
    { opDefs = optimizeOps passes <$> opDefs
    , topLevel = optimizeOps passes topLevel
    }
