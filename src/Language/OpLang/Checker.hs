module Language.OpLang.Checker(check) where

import Data.Maybe(fromJust)
import Data.List((\\), nub)
import Data.Bifunctor(first)
import Control.Monad((<=<), join)
import Text.Printf(printf)

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Language.OpLang.IR(Defs, Name, Def, calledOps)

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
    undefinedOps = M.filter (not . null) $ M.mapWithKey (curry undefinedCalls) defs

    undefinedCalls :: Def -> [Name]
    undefinedCalls def = nub $ filter (not . (`M.member` defs)) $ calledOps def

check :: [Def] -> Either String Defs
check = first (unlines . (show <$>)) . (checkUndefinedCalls <=< checkDuplicateDefs)
