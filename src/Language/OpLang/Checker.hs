module Language.OpLang.Checker(check) where

import Control.Monad((<=<), join)
import Data.Bifunctor(first)
import Data.List((\\), nub)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Maybe(fromJust)
import Text.Printf(printf)

import Language.OpLang.IR(Defs, Name, Def, calledOps, Body)

data Error
  = DuplicateDefinition Char
  | UndefinedCall Name Char

instance Show Error where
  show = printf "Error: %s." . go
    where
      go :: Error -> String
      go (DuplicateDefinition op) = printf "Duplicate definition of operator '%c'" op
      go (UndefinedCall caller callee) = printf "Call to undefined operator '%c' in %s" callee $ fmtName caller

      fmtName :: Name -> String
      fmtName = maybe "top level" (printf "body of '%c'")

type Check a = Either [Error] a

checkDuplicateDefs :: [Def] -> Check Defs
checkDuplicateDefs defs =
  case names \\ nub names of
    [] -> pure $ M.fromList defs
    duplicates -> Left $ DuplicateDefinition . fromJust <$> nub duplicates
  where
    names = fst <$> defs

checkUndefinedCalls :: Defs -> Check Defs
checkUndefinedCalls defs =
  if M.null undefinedOps
    then pure defs
    else Left $ join $ M.elems $ M.mapWithKey (\k ns -> UndefinedCall k . fromJust <$> ns) undefinedOps
  where
    undefinedOps :: Map Name [Name]
    undefinedOps = M.filter (not . null) $ M.map undefinedCalls defs

    undefinedCalls :: Body -> [Name]
    undefinedCalls body = nub $ filter (not . (`M.member` defs)) $ calledOps body

check :: [Def] -> Either String Defs
check = first (unlines . (show <$>)) . (checkUndefinedCalls <=< checkDuplicateDefs)
