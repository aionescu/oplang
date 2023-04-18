module Language.OpLang.Optimizer(optimize) where

import Language.OpLang.Syntax

syncTally :: Bool -> Val -> Offset -> [Instr] -> [Instr]
syncTally False 0 _ acc = acc
syncTally False n offset acc = Add n offset : acc
syncTally True n offset acc = Set n offset : acc

syncOffset :: Offset -> [Instr] -> [Instr]
syncOffset 0 acc = acc
syncOffset n acc = Move n : acc

syncAll :: Bool -> Val -> Offset -> [Instr] -> [Instr]
syncAll known tally offset = syncTally known tally 0 . syncOffset offset

removeSet0 :: [Instr] -> [Instr]
removeSet0 (Set 0 0 : ops) = ops
removeSet0 ops = ops

optimizeOps :: [Op] -> [Instr]
optimizeOps = removeSet0 . go False True 0 0 []
  where
    go :: Bool -> Bool -> Val -> Offset -> [Instr] -> [Op] -> [Instr]
    go False _ _ _ acc [] = reverse acc
    go True known tally offset acc [] = reverse $ syncAll known tally offset acc
    go loop known tally offset acc (op : ops) =
      case op of
        Incr -> go loop known (tally + 1) offset acc ops
        Decr -> go loop known (tally - 1) offset acc ops
        Read' -> go loop False 0 offset (Read offset : acc) ops
        Pop' -> go loop False 0 offset (Pop offset : acc) ops

        MoveL -> go loop False 0 (offset - 1) (syncTally known tally offset acc) ops
        MoveR -> go loop False 0 (offset + 1) (syncTally known tally offset acc) ops
        Write' -> go loop False 0 offset (Write offset : syncTally known tally offset acc) ops
        Push' -> go loop False 0 offset (Push offset : syncTally known tally offset acc) ops
        Call' c -> go loop False 0 offset (Call c : syncTally known tally offset acc) ops

        Loop' _ | (True, 0) <- (known, tally) -> go loop known tally offset acc ops
        Loop' l ->
          case go True False 0 0 [] l of
            [Add 1 0] -> go loop True 0 offset acc ops
            [Add -1 0] -> go loop True 0 offset acc ops

            [Add 1 o] -> go loop known tally offset (Set 0 (offset + o) : acc) ops
            [Add -1 o] -> go loop known tally offset (Set 0 (offset + o) : acc) ops

            [Add n o, Add -1 0] -> go loop False 0 offset (AddCell n (o + offset) offset : syncTally known tally offset acc) ops
            [Add -1 0, Add n o] -> go loop False 0 offset (AddCell n (o + offset) offset : syncTally known tally offset acc) ops

            l' -> go loop False 0 0 (Loop l' : syncAll known tally offset acc) ops

optimize :: Program Op -> Program Instr
optimize Program{..} =
  Program
  { opDefs = optimizeOps <$> opDefs
  , topLevel = optimizeOps topLevel
  }
