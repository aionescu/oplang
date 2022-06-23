module Language.OpLang.Optimize(optimize) where

import Language.OpLang.IR

syncAdd :: Bool -> Val -> Offset -> [Instr] -> [Instr]
syncAdd False 0 _ acc = acc
syncAdd False n offset acc = Add n offset : acc
syncAdd True n offset acc = Set n offset : acc

syncMove :: Offset -> [Instr] -> [Instr]
syncMove 0 acc = acc
syncMove n acc = Move n : acc

optimizeOps :: [Op] -> [Instr]
optimizeOps = go False True 0 0 []
  where
    go :: Bool -> Bool -> Val -> Offset -> [Instr] -> [Op] -> [Instr]
    go False _ _ _ acc [] = reverse acc
    go True known tally offset acc [] = reverse $ syncAdd known tally 0 $ syncMove offset acc
    go loop known tally offset acc (op : ops) =
      case op of
        Incr -> go loop known (tally + 1) offset acc ops
        Decr -> go loop known (tally - 1) offset acc ops
        Loop' [Incr] -> go loop True 0 offset acc ops
        Loop' [Decr] -> go loop True 0 offset acc ops
        Read' -> go loop False 0 offset (Read offset : acc) ops
        Pop' -> go loop False 0 offset (Pop offset : acc) ops

        MoveL -> go loop False 0 (offset - 1) acc' ops
        MoveR -> go loop False 0 (offset + 1) acc' ops
        Write' -> go loop False 0 offset (Write offset : acc') ops
        Push' -> go loop False 0 offset (Push offset : acc') ops
        Call' c -> go loop False 0 offset (Call c : acc') ops

        Loop' l -> go loop False 0 0 (goLoop l : syncMove offset acc') ops
      where
        acc' = syncAdd known tally offset acc

    goLoop :: [Op] -> Instr
    goLoop ops =
      case go True False 0 0 [] ops of
        [Add n o, Add -1 0] -> AddCell n o
        [Add -1 0, Add n o] -> AddCell n o
        l -> Loop l

optimize :: Program Op -> Program Instr
optimize Program{..} =
  Program
  { opDefs = optimizeOps <$> opDefs
  , topLevel = optimizeOps topLevel
  }
