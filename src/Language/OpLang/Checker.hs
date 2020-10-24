module Language.OpLang.Checker(check) where

import Control.Monad((<=<), join)
import Data.List((\\), nub)
import Data.Maybe(fromJust)
import Text.Printf(printf)

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import Language.OpLang.AST(mapLeft, err, OpLang, Dict, Name, Def, DefList, calledOps)

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

checkDuplicateDefs :: DefList -> Check Dict
checkDuplicateDefs defs =
  case names \\ nub names of
    [] -> pure $ HM.fromList defs
    duplicates -> err $ DuplicateDefinition . fromJust <$> nub duplicates
  where
    names = fst <$> defs

checkUndefinedCalls :: Dict -> Check Dict
checkUndefinedCalls defs =
  if HM.null undefinedOps
    then pure defs
    else err $ join $ HM.elems $ HM.mapWithKey (\k ns -> UndefinedCall k . fromJust <$> ns) undefinedOps
  where
    undefinedOps ::HashMap Name [Name]
    undefinedOps = HM.filter (not . null) $ HM.mapWithKey (curry undefinedCalls) defs

    undefinedCalls :: Def -> [Name]
    undefinedCalls def = nub $ filter (not . (`HM.member` defs)) $ calledOps def

check :: DefList -> OpLang Dict
check = mapLeft (unlines . (show <$>)) . (checkUndefinedCalls <=< checkDuplicateDefs)
