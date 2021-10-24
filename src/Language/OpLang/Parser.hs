module Language.OpLang.Parser(parse) where

import Control.Applicative(empty)
import Control.Monad.Writer.Strict(tell)
import Data.Functor(($>))
import Data.List(intercalate)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text(Text)
import Text.Parsec hiding (parse)

import Language.OpLang.Comp
import Language.OpLang.Syntax
import Utils

type Parser = Parsec Text ()

reserved :: [Char]
reserved = "+-<>,.;:[]{}# \t\r\n"

comment :: Parser ()
comment = char '#' *> manyTill anyChar (endOfLine $> () <|> eof) $> ()

ws :: Parser ()
ws = spaces *> skipMany (comment *> spaces)

intrinsic :: Parser Op
intrinsic =
  choice
  [ char '+' $> Add 1
  , char '-' $> Add (-1)
  , char '<' $> Move (-1)
  , char '>' $> Move 1
  , char ',' $> Read
  , char '.' $> Write 1
  , char ';' $> Pop 1
  , char ':' $> Push
  ]

block :: Char -> Char -> Parser [Op]
block b e = between (char b *> ws) (char e *> ws) $ many op

loop :: Parser Op
loop = Loop <$> block '[' ']'

custom :: Parser Char
custom = noneOf reserved

op :: Parser Op
op = choice [try loop, intrinsic, Call <$> custom] <* ws

def :: Parser (Id, [Op])
def = (,) <$> (custom <* ws) <*> block '{' '}'

defs :: Parser (Map Id [Op])
defs = many (try def) >>= toMap
  where
    toMap ds
      | unique = pure $ M.fromList ds
      | otherwise = fail $ "Duplicate definition of operators: " <> intercalate ", " (show <$> S.toList set)
      where
        ids = fst <$> ds
        set = S.fromList ids
        unique = S.size set == length ids

program :: Parser Program
program = ws *> (Program <$> defs <*> many op) <* eof

parse :: Text -> Comp Program
parse code =
  case runParser program () "" code of
    Left e -> tell [showT e] *> empty
    Right p -> pure p
