module Language.OpLang.Optimizer(optimize) where

import Control.Category((>>>))
import Control.Monad.Reader(asks)

import Comp(Comp)
import Opts(Opts(..))
import Language.OpLang.IR(Program(..), Op(..), NoOff, Off)
import Control.Monad.State.Strict (State, get, modify', put, evalState)
import Data.Functor (($>))
import Data.Foldable (fold)

peephole :: [Op NoOff] -> (Bool, [Op NoOff])
peephole = go False []
  where
    go :: Bool -> [Op NoOff] -> [Op NoOff] -> (Bool, [Op NoOff])
    go changed acc ops' =
      case ops' of
        Add _ 0 : ops -> go True acc ops
        Move 0 : ops -> go True acc ops
        Pop _ 0 : ops -> go True acc ops

        Loop [Add _ -1] : ops -> go True acc (Set () 0 : ops)
        Loop [Add _ 1] : ops -> go True acc (Set () 0 : ops)

        Set _ s : Add _ a : ops -> go True acc (Set () (s + a) : ops)
        Add _ a : Add _ b : ops -> go True acc (Add () (a + b) : ops)
        Move a : Move b : ops -> go True acc (Move (a + b) : ops)
        Pop _ a : Pop _ b : ops -> go True acc (Pop () (a + b) : ops)
        Write _ a : Write _ b : ops -> go True acc (Write () (a + b) : ops)

        Add{} : ops@(Read{} : _) -> go True acc ops
        Add{} : ops@(Pop{} : _) -> go True acc ops
        Add{} : ops@(Set{} : _) -> go True acc ops

        Set{} : ops@(Read{} : _) -> go True acc ops
        Set{} : ops@(Pop{} : _) -> go True acc ops
        Set{} : ops@(Set{} : _) -> go True acc ops

        Pop _ n : Push{} : ops -> go True acc (Pop () (n - 1) : Peek () : ops)
        Push{} : Pop _ n : ops -> go True acc (Pop () (n - 1) : ops)

        Set _ 0 : Loop _ : ops -> go True acc (Set () 0 : ops)
        l@(Loop _) : Loop _ : ops -> go True acc (l : ops)
        Loop [l@(Loop _)] : ops -> go True acc (l : ops)
        Loop l : ops ->
          let (changed', l') = go False [] l
          in go changed' (Loop l' : acc) ops

        op : ops -> go changed (op : acc) ops
        [] -> (changed, reverse acc)

peepholeN :: Word -> [Op NoOff] -> [Op NoOff]
peepholeN 0 ops = ops
peepholeN n ops =
  if changed
  then peepholeN (n - 1) ops'
  else ops'

  where
    (changed, ops') = peephole ops

removeSet0 :: [Op NoOff] -> [Op NoOff]
removeSet0 (Set _ 0 : ops) = ops
removeSet0 ops = ops

sync :: State Off [Op Off]
sync = get >>= \case
  0 -> pure []
  n -> put 0 $> [Move n]

withOffsetLoop :: [Op NoOff] -> State Off (Op Off)
withOffsetLoop ops = (\a b -> Loop $ fold a <> b) <$> traverse withOffset ops <*> sync

withOffsetBody :: [Op NoOff] -> [Op Off]
withOffsetBody = fold . (`evalState` 0) . traverse withOffset

withOffset :: Op NoOff -> State Off [Op Off]
withOffset op = do
  off <- get
  case op of
    Add _ n -> pure [Add off n]
    Set _ n -> pure [Set off n]
    Pop _ n -> pure [Pop off n]
    Push _ -> pure [Push off]
    Peek _ -> pure [Peek off]
    Read _ -> pure [Read off]
    Write _ n -> pure [Write off n]
    Move n -> modify' (+ n) $> []
    AddTimes{} -> error "withOffset: Unreachable"
    Loop ops -> (\a b -> a <> [b]) <$> sync <*> withOffsetLoop ops
    Call c -> pure [Call c]

simplifyAddTimes :: Op Off -> Op Off
simplifyAddTimes (Loop [Add o n, Add 0 -1]) = AddTimes o n
simplifyAddTimes (Loop ops) = Loop $ simplifyAddTimes <$> ops
simplifyAddTimes op = op

optimizeBody :: Word -> [Op NoOff] -> [Op Off]
optimizeBody passes ops =
  simplifyAddTimes
  <$> withOffsetBody (removeSet0 $ peepholeN passes $ Set () 0 : ops)

optimize :: Program NoOff -> Comp (Program Off)
optimize Program{..} =
  asks $ optsOptPasses >>> \passes ->
    Program
    { opDefs = optimizeBody passes <$> opDefs
    , topLevel = optimizeBody passes topLevel
    }
