module Checker(check) where

import Data.List(intercalate, nub)
import Data.Maybe(catMaybes)

import Ast

calledOps :: [Op] -> [Char]
calledOps = nub . go
  where
    go (OpCall c : rest) = c : calledOps rest
    go (TailCall c : rest) = c : calledOps rest
    go (Loop l : rest) = calledOps l ++ calledOps rest
    go (_ : rest) = calledOps rest
    go [] = []

checkCalls :: [Char] -> [Op] -> Bool
checkCalls defined ops = all (`elem` defined) $ calledOps ops

checkDefs :: [Def] -> [Char]
checkDefs defs = catMaybes $ go defined <$> defs
  where
    defined = defName <$> defs

    go defs (Def name body) =
      if checkCalls defs body
      then Nothing
      else Just name

checkProgram :: Program -> Maybe String
checkProgram (Program defs topLevel) = maybeify $ intercalate "\n" (msgify (checkDefs defs) ++ go topLevel)
  where
    maybeify "" = Nothing
    maybeify s = Just s

    msgify chars = msgify1 <$> chars
    msgify1 char = "Call to undefined operator in body of '" ++ [char] ++ "'."

    go ops =
      if checkCalls (defName <$> defs) ops
      then []
      else ["Call to undefined operator in toplevel."]

check :: Program -> Either String Program
check program =
  case checkProgram program of
    Nothing -> Right program
    Just err -> Left err