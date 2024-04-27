module Language.OpLang.Optimizer(optimize) where

import Data.IntMap.Strict(IntMap)
import Data.IntMap.Strict qualified as I
import Data.IntSet qualified as S

import Language.OpLang.Syntax

-- Models partial information about a memory cell, used during partial evaluation.
data Cell
  = UnknownPlus Val -- Unknown starting value + a known constant.
  | Known Bool Val -- Fully-known constant value. The 'Bool' tracks whether it should be committed unconditionally.
  deriving stock Eq

isKnown :: Cell -> Bool
isKnown UnknownPlus{} = False
isKnown _ = True

getVal :: Cell -> Val
getVal (UnknownPlus val) = val
getVal (Known _ val) = val

-- 'isKnown' and 'getVal' could be made into lenses, which would make this definition redundant,
-- but adding a dependency on 'lens' just for this would be overkill.
mapVal :: (Val -> Val) -> Cell -> Cell
mapVal f (UnknownPlus val) = UnknownPlus $ f val
mapVal f (Known c val) = Known c $ f val

instance Semigroup Cell where
  (<>) :: Cell -> Cell -> Cell
  UnknownPlus a <> UnknownPlus b = UnknownPlus (a + b)
  UnknownPlus a <> Known c b = Known c (a + b)
  Known c a <> _ = Known c a

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
    UnknownPlus val -> Add offset val : acc

    Known False 0 -> acc
    Known _ val -> Set offset val : acc

-- Commit a Move instruction.
commitMove :: Offset -> [Instr] -> [Instr]
commitMove 0 acc = acc
commitMove offset acc = Move offset : acc

-- Commit cell update instructions for all the cells in the specified tape segment.
commitCells :: Tape -> [Instr] -> [Instr]
commitCells tape acc = I.foldrWithKey' commitCell acc tape

-- Commit a set of AddMul instructions, resulting from a for-like loop.
commitAddMuls :: Offset -> Val -> IntMap Val -> [Instr] -> [Instr]
commitAddMuls offset diff vals acc = I.foldrWithKey' (commitAddMul offset diff) acc vals
  where
    commitAddMul :: Offset -> Val -> Offset -> Val -> [Instr] -> [Instr]
    commitAddMul _ _ _ 0 acc = acc
    commitAddMul initialOffset diff offset val acc = AddMul offset initialOffset (val * -diff) : acc

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

    Write' ->
      case I.findWithDefault mempty offset tape of
        Known _ val -> pEval offset tape (WriteKnown val : acc) ops
        cell -> pEval offset (I.delete offset tape) (Write offset : commitCell offset cell acc) ops

    Push' ->
      case I.findWithDefault mempty offset tape of
        Known _ val -> pEval offset tape (PushKnown val : acc) ops
        cell -> pEval offset (I.delete offset tape) (Push offset : commitCell offset cell acc) ops

    Call' c -> pEval offset tape (Call c : acc) ops

    Loop' l ->
      case I.findWithDefault mempty offset tape of
        Known _ 0 -> pEval offset tape acc ops

        cell ->
          case pEval 0 I.empty [] l of
            (0, I.mapKeys (offset +) -> tape', []) | Just (UnknownPlus diff) <- tape' I.!? offset, abs diff == 1, not $ any isKnown tape' ->
              case cell of
                Known _ val -> pEval offset (I.insert offset (Known True 0) $ I.unionWith (<>) (mapVal ((val * -diff) *) <$> tape') tape) acc ops
                _ ->
                  let
                    modified = I.intersection tape tape'
                    addMuls = getVal <$> I.delete offset tape'
                    remaining = tape I.\\ modified
                  in
                    pEval offset (I.insert offset (Known True 0) remaining) (commitAddMuls offset diff addMuls $ commitCells modified acc) ops

            (offset', tape', acc') ->
              let l' = Loop $ reverse $ commitMove offset' $ commitCells tape' acc'
              in pEval 0 (I.singleton 0 $ Known False 0) (l' : commitMove offset (commitCells tape acc)) ops

optimizeOps :: Word -> [Op] -> [Instr]
optimizeOps tapeSize ops = reverse acc
  where
    initialTape = I.fromSet (const $ Known False 0) $ S.fromRange (0, fromEnum tapeSize - 1)
    (_, _, acc) = pEval 0 initialTape [] ops

optimize :: Word -> Program Op -> Program Instr
optimize tapeSize Program{..} =
  Program
  { opDefs = optimizeOps tapeSize <$> opDefs
  , topLevel = optimizeOps tapeSize topLevel
  }
