module Language.OpLang.Parser(parse) where

import Data.Bifunctor(first)
import Data.Functor(($>))
import Data.Text(Text)
import Text.Parsec hiding (parse)

import Language.OpLang.IR

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
op = choice [try loop, intrinsic, OpCall . Just <$> custom] <* ws

def :: Parser Def
def = (,) . Just <$> (custom <* ws) <*> block '{' '}'

topLevel :: Parser Def
topLevel = (Nothing,) <$> many op

program :: Parser [Def]
program = ws *> (flip (:) <$> many (try def) <*> topLevel) <* eof

parse :: Text -> Either String [Def]
parse = first show . runParser program () ""
