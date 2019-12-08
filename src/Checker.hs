module Checker(check) where

import Data.List
import Data.Maybe
import Ast

checkCalls :: [Char] -> [Op] -> Bool
checkCalls defined ops = all (`elem` defined) $ calledOps ops

checkDefs :: [OpDef] -> [Char]
checkDefs defs = catMaybes $ go defined <$> defs
  where
    defined = opDefName <$> defs

    go defs (OpDef name body) =
      if checkCalls defs body
      then Nothing
      else Just name

checkProgram :: Program -> Maybe String
checkProgram (Program opDefs topLevel) = maybeify $ intercalate "\n" (msgify (checkDefs opDefs) ++ go topLevel)
  where
    maybeify "" = Nothing
    maybeify s = Just s

    msgify chars = msgifySingle <$> chars
    msgifySingle char = "Call to undefined operator in body of '" ++ [char] ++ "'."

    go ops =
      if checkCalls (opDefName <$> opDefs) ops
      then []
      else ["Call to undefined operator in toplevel."]

check :: Program -> Either String Program
check program =
  case checkProgram program of
    Nothing -> Right program
    Just err -> Left err