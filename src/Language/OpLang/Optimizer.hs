module Language.OpLang.Optimizer(optimize) where

import Control.Category((>>>))
import Control.Monad.Reader(asks)
import Control.Monad.ST(ST, runST)
import Data.Foldable(fold)
import Data.Functor(($>))
import Data.STRef(STRef, readSTRef, newSTRef, writeSTRef, modifySTRef')

import Opts(Opts(..))
import Comp(Comp)
import Language.OpLang.IR(Program(..), Op(..), NoOff, Off)

peephole :: [Op NoOff] -> (# Bool, [Op NoOff] #)
peephole = go False []
  where
    go :: Bool -> [Op NoOff] -> [Op NoOff] -> (# Bool, [Op NoOff] #)
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
          let (# changed', l' #) = go False [] l
          in go changed' (Loop l' : acc) ops

        op : ops -> go changed (op : acc) ops
        [] -> (# changed, reverse acc #)

peepholeN :: Word -> [Op NoOff] -> [Op NoOff]
peepholeN 0 ops = ops
peepholeN n ops =
  if changed
  then peepholeN (n - 1) ops'
  else ops'

  where
    (# changed, ops' #) = peephole ops

removeSet0 :: [Op NoOff] -> [Op NoOff]
removeSet0 (Set _ 0 : ops) = ops
removeSet0 ops = ops

sync :: STRef s Off -> ST s [Op Off]
sync r = readSTRef r >>= \case
  0 -> pure []
  n -> writeSTRef r 0 $> [Move n]

withOffsetLoop :: STRef s Off -> [Op NoOff] -> ST s (Op Off)
withOffsetLoop r ops = (\a b -> mkLoop $ fold a <> b) <$> traverse (withOffset r) ops <*> sync r
  where
    mkLoop [Add o n, Add 0 -1] = AddTimes o n
    mkLoop [Add 0 -1, Add o n] = AddTimes o n
    mkLoop l = Loop l

withOffsetBody :: [Op NoOff] -> [Op Off]
withOffsetBody ops = fold $ runST do
  r <- newSTRef 0
  traverse (withOffset r) ops

withOffset :: STRef s Off -> Op NoOff -> ST s [Op Off]
withOffset r op = readSTRef r >>= \off ->
  case op of
    Add _ n -> pure [Add off n]
    Set _ n -> pure [Set off n]
    Pop _ n -> pure [Pop off n]
    Push _ -> pure [Push off]
    Peek _ -> pure [Peek off]
    Read _ -> pure [Read off]
    Write _ n -> pure [Write off n]
    Move n -> modifySTRef' r (+ n) $> []
    AddTimes{} -> error "withOffset: Unreachable"
    Loop ops -> (\a b -> a <> [b]) <$> sync r <*> withOffsetLoop r ops
    Call c -> pure [Call c]

optimizeBody :: Word -> [Op NoOff] -> [Op Off]
optimizeBody passes ops = withOffsetBody (removeSet0 $ peepholeN passes $ Set () 0 : ops)

optimize :: Program NoOff -> Comp (Program Off)
optimize Program{..} =
  asks $ optsOptPasses >>> \passes ->
    Program
    { opDefs = optimizeBody passes <$> opDefs
    , topLevel = optimizeBody passes topLevel
    }
