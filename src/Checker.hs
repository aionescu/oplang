module Checker(check) where

import Control.Monad(join)
import Data.List((\\), nub)
import Data.Maybe(fromJust)

import Data.Text(Text)
import qualified Data.Text as T

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap

import AST(Dict, Body, Name, DefList, calledOps)

illegalCalls :: Dict -> Body -> [Name]
illegalCalls d body = nub $ filter (not . (`HashMap.member` d)) $ calledOps body

illegalBodies :: Dict -> HashMap Name [Name]
illegalBodies d = HashMap.filter (not . null) $ illegalCalls d <$> d

errorMsgs :: HashMap Name [Name] -> HashMap Name [Text]
errorMsgs d = HashMap.mapWithKey errorMsg d
  where
    errorMsg name ops = errorMsg1 name <$> ops
    errorMsg1 name op = "Error: Call to undefined operator '" <> T.singleton (fromJust op) <> "' in " <> fromName name <> "."

    fromName Nothing = "top level"
    fromName (Just n) = "body of '" <> T.singleton n <> T.singleton '\''

checkDups :: DefList -> Either Text Dict
checkDups l =
  let
    l' = fst <$> l
    uniq = nub l'
    toMsg a = "Error: Duplicate definition of operator '" <> T.singleton a <> "'."
    concatDups = T.intercalate "\n" . map (toMsg . fromJust)
  in
    case l' \\ uniq of
      [] -> Right $ HashMap.fromList l
      dups -> Left $ concatDups $ nub dups

checkUndefs :: Dict -> Either Text Dict
checkUndefs d =
  let msgs = join $ HashMap.elems $ errorMsgs $ illegalBodies d
  in
    case msgs of
      [] -> Right d
      l -> Left (T.intercalate "\n" l)

check :: DefList -> Either Text Dict
check dl = checkDups dl >>= checkUndefs