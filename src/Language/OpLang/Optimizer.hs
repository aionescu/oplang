module Language.OpLang.Optimizer(optimize) where

import Data.IntMap.Strict(IntMap)
import Data.IntMap.Strict qualified as I
import Data.IntSet qualified as S

import Language.OpLang.Syntax

-- Models partial information about a memory cell, used during partial evaluation.
data Cell
  = UnknownPlus Val -- Unknown starting value + a known constant.
  | Known Val -- Fully-known constant value.
  | Known' Val -- Same as Known, but ensure it always gets committed.
  deriving stock Eq

isKnown :: Cell -> Bool
isKnown UnknownPlus{} = False
isKnown _ = True

getVal :: Cell -> Val
getVal (UnknownPlus val) = val
getVal (Known val) = val
getVal (Known' val) = val

-- 'isKnown' and 'getVal' could be made into lenses, which would make this definition redundant,
-- but adding a dependency on 'lens' just for this would be overkill.
mapVal :: (Val -> Val) -> Cell -> Cell
mapVal f (UnknownPlus val) = UnknownPlus $ f val
mapVal f (Known val) = Known $ f val
mapVal f (Known' val) = Known' $ f val

instance Semigroup Cell where
  (<>) :: Cell -> Cell -> Cell
  UnknownPlus a <> UnknownPlus b = UnknownPlus (a + b)
  UnknownPlus a <> Known b = Known (a + b)
  UnknownPlus a <> Known' b = Known' (a + b)
  Known a <> _ = Known a
  Known' a <> _ = Known' a

instance Monoid Cell where
  mempty :: Cell
  mempty = UnknownPlus 0

-- Models partial information about a segment of the memory tape.
-- Represented as a mapping from memory offsets to Cells.
type Tape = IntMap Cell

-- Commit a cell update instruction.
commitCell :: Offset -> Cell -> [Instr] -> [Instr]
commitCell offset cell acc =
  case cell of
    UnknownPlus 0 -> acc
    Known 0 -> acc

    UnknownPlus val -> Add offset val : acc
    Known val -> Set offset val : acc
    Known' val -> Set offset val : acc

-- Commit a Move instruction.
commitMove :: Offset -> [Instr] -> [Instr]
commitMove 0 acc = acc
commitMove offset acc = Move offset : acc

-- Commit cell update instructions for all the cells in the specified tape segment.
commitCells :: Tape -> [Instr] -> [Instr]
commitCells tape acc = I.foldrWithKey' commitCell acc tape

-- Commit a set of AddMul instructions, resulting from a for-like loop.
commitAddMuls :: Offset -> Val -> IntMap Val -> [Instr] -> [Instr]
commitAddMuls offset loopVal vals acc = I.foldrWithKey' (commitAddMul offset loopVal) acc vals
  where
    commitAddMul :: Offset -> Val -> Offset -> Val -> [Instr] -> [Instr]
    commitAddMul _ _ _ 0 acc = acc
    commitAddMul initialOffset loopVal offset val acc = AddMul offset initialOffset (val * -loopVal) : acc

-- Partially evaluate a program, producing an optimized sequence of instructions.
pEval :: Offset -> Tape -> [Instr] -> [Op] -> (Offset, Tape, [Instr])
pEval offset tape acc [] = (offset, tape, acc)
pEval offset tape acc (op : ops) =
  case op of
    Incr -> pEval offset (I.insertWith (<>) offset (UnknownPlus 1) tape) acc ops
    Decr -> pEval offset (I.insertWith (<>) offset (UnknownPlus -1) tape) acc ops

    Read' -> pEval offset (I.delete offset tape) (Read offset : acc) ops
    Pop' -> pEval offset (I.delete offset tape) (Pop offset : acc) ops

    MoveL -> pEval (offset - 1) tape acc ops
    MoveR -> pEval (offset + 1) tape acc ops

    Write' -> pEval offset (I.delete offset tape) (Write offset : commitCell offset (I.findWithDefault mempty offset tape) acc) ops
    Push' -> pEval offset (I.delete offset tape) (Push offset : commitCell offset (I.findWithDefault mempty offset tape) acc) ops

    Call' c -> pEval offset tape (Call c : acc) ops

    Loop' l ->
      case I.findWithDefault mempty offset tape of
        Known 0 -> pEval offset tape acc ops
        Known' 0 -> pEval offset tape acc ops

        cell ->
          case pEval 0 I.empty [] l of
            (0, tape', []) | Just (UnknownPlus v) <- tape' I.!? 0, abs v == 1, not $ any isKnown tape' ->
              let
                val = getVal cell
                newTape = I.mapKeys (offset +) tape'
              in
                case isKnown cell of
                  True -> pEval offset (I.insert offset (Known' 0) $ I.unionWith (<>) (mapVal ((val * -v) *) <$> newTape) tape) acc ops
                  False ->
                    let
                      modified = I.intersection tape newTape
                      addMuls = getVal <$> I.delete offset newTape
                      remaining = tape I.\\ modified
                    in
                      pEval offset (I.insert offset (Known' 0) remaining) (commitAddMuls offset v addMuls (commitCells modified acc)) ops

            (offset', tape', acc') ->
              let l' = Loop $ reverse $ commitMove offset' $ commitCells tape' acc'
              in pEval 0 (I.singleton 0 (Known 0)) (l' : commitMove offset (commitCells tape acc)) ops

optimizeOps :: Word -> [Op] -> [Instr]
optimizeOps tapeSize ops = reverse acc
  where
    initialTape = I.fromSet (const $ Known 0) $ S.fromRange (0, fromEnum tapeSize - 1)
    (_, _, acc) = pEval 0 initialTape [] ops

optimize :: Word -> Program Op -> Program Instr
optimize tapeSize Program{..} =
  Program
  { opDefs = optimizeOps tapeSize <$> opDefs
  , topLevel = optimizeOps tapeSize topLevel
  }
