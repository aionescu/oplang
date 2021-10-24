module Language.OpLang.Checker(check) where

import Control.Monad(guard)
import Control.Monad.Writer.Strict(tell)
import Data.Bifunctor(bimap, second)
import Data.Functor(($>))
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text qualified as T

import Language.OpLang.Comp
import Language.OpLang.Syntax
import Utils

checkUndefinedCalls :: Program -> Comp ()
checkUndefinedCalls Program{..} = tell errors *> guard (null errors)
  where
    defined = M.keysSet opDefs

    undefinedInTopLevel = (Nothing, calledOps topLevel S.\\ defined)
    undefinedInDefs = bimap Just ((S.\\ defined) . calledOps) <$> M.toList opDefs

    toMsg (Nothing, ops) = ("Error (in top level): Calls to undefined operator " <>) . showT <$> ops
    toMsg (Just i, ops) =
      (("Error (in definition of " <> showT i <> "): Calls to undefined operator ") <>) . showT <$> ops

    errors = toMsg . second S.toList =<< undefinedInTopLevel : undefinedInDefs

allUsedOps :: Map Id [Op] -> Set Id -> [Op] -> Set Id
allUsedOps defs seen ops
  | S.null used = seen
  | otherwise = foldMap (allUsedOps defs (seen <> used) . (defs M.!)) used
  where
    used = calledOps ops S.\\ seen

removeUnusedOps :: Program -> Comp Program
removeUnusedOps p@Program{..} =
  tell warning $> p { opDefs = usedDefs }
  where
    warning =
      [ "Warning: Unused operators: " <> T.intercalate ", " (showT <$> M.keys unusedDefs)
      | not $ M.null unusedDefs
      ]

    unusedDefs = opDefs M.\\ usedDefs
    usedDefs = M.restrictKeys opDefs usedOps
    usedOps = allUsedOps opDefs S.empty topLevel

check :: Program -> Comp Program
check p = checkUndefinedCalls p *> removeUnusedOps p
