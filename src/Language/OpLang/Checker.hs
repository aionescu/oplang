module Language.OpLang.Checker(check) where

import Data.Bifunctor(bimap, first, second)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T

import Language.OpLang.Syntax
import Utils

checkUndefinedCalls :: Program -> Either [Text] Program
checkUndefinedCalls p@Program{..}
  | [] <- errors = pure p
  | otherwise = Left errors
  where
    defined = M.keysSet opDefs

    undefinedInTopLevel = (Nothing, calledOps topLevel S.\\ defined)
    undefinedInDefs = bimap Just ((S.\\ defined) . calledOps) <$> M.toList opDefs

    toMsg (Nothing, ops) = ("Error (in top level): Calls to undefined operator " <>) . showT <$> ops
    toMsg (Just i, ops) =
      (("Error (in definition of " <> showT i <> "): Calls to undefined operator ") <>) . showT <$> ops

    errors = toMsg . second S.toList =<< undefinedInTopLevel : undefinedInDefs

check :: Program -> Either Text Program
check = first T.unlines . checkUndefinedCalls
