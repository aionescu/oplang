{-# LANGUAGE LambdaCase #-}

module Checker(check) where

import Control.Monad(join)
import Data.List(filter, intercalate, nub, null)
import Data.Maybe(catMaybes, fromJust)

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap

import Ast

illegalCalls :: Dict -> Body -> [Name]
illegalCalls d body = nub $ filter (not . (`HashMap.member` d)) $ calledOps body

illegalBodies :: Dict -> HashMap Name [Name]
illegalBodies d = HashMap.filter (not . null) $ HashMap.map (illegalCalls d) d

errorMsgs :: HashMap Name [Name] -> HashMap Name [String]
errorMsgs d = HashMap.mapWithKey errorMsg d
  where
    errorMsg name ops = errorMsg1 name <$> ops
    errorMsg1 name op = "Error: Call to undefined operator '" ++ [fromJust op] ++ "' in " ++ fromName name ++ "."

    fromName Nothing = "top level"
    fromName (Just n) = "body of '" ++ [n, '\'']

check :: Dict -> Either String Dict
check d =
  let msgs = join $ HashMap.elems $ errorMsgs $ illegalBodies d
  in
    case msgs of
      [] -> Right d
      l -> Left (intercalate "\n" l)