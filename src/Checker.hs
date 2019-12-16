{-# LANGUAGE LambdaCase #-}

module Checker(check) where

import Data.List(intercalate)
import Data.Maybe(catMaybes)

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap

import Ast

isLegal :: Dict -> Body -> Bool
isLegal d body = all (`HashMap.member` d) $ calledOps body

getIllegalBodies :: Dict -> Dict
getIllegalBodies d = HashMap.filter (not . isLegal d) d

getErrorMsgs :: Dict -> [String]
getErrorMsgs = (toErrorMsg <$>) . HashMap.keys
  where
    toErrorMsg name = "Call to undefined operator in body of " ++ fromName name

check :: Dict -> Either String Dict
check d =
  let msgs = getErrorMsgs $ getIllegalBodies d
  in
    case msgs of
      [] -> Right d
      l -> Left (intercalate "\n" l)