module Parser(parse) where

import Control.Applicative((<|>))
import Control.Exception(assert)
import Text.Parsec(between, eof, many, manyTill, Parsec, runParser, skipMany, try)
import Text.Parsec.Char(anyChar, char, endOfLine, noneOf, oneOf, space)

import Ast

type Parser = Parsec String ()

skip :: Parser a -> Parser ()
skip p = do
  p
  pure ()

comment :: Parser ()
comment = do
  char '#'
  manyTill anyChar (skip endOfLine <|> eof)
  pure ()

justWs :: Parser ()
justWs = skipMany (skip space <|> comment)

ws :: Parser a -> Parser a
ws p = do
  r <- p
  justWs  
  pure r

intrinsics :: String
intrinsics = "+-<>,.;:"

reserved :: String
reserved = intrinsics ++ "[]{}# \t\r\n"

intrinsic :: Parser Op
intrinsic = go <$> oneOf intrinsics
  where
    go '+' = Add 1
    go '-' = Add (-1)
    go '<' = Move (-1)
    go '>' = Move 1
    go ',' = Read
    go '.' = Write
    go ';' = Pop 1
    go ':' = Push
    go _ = error "Impossible intrinsic op in parser."

loop :: Parser Op
loop = Loop <$> between (ws $ char '[') (char ']') (many $ ws op)

custom :: Parser Char
custom = noneOf reserved

op :: Parser Op
op = intrinsic <|> loop <|> (OpCall <$> custom)

opDef :: Parser Def
opDef = do
  name <- ws custom
  ws $ char '{'
  body <- many $ ws op
  char '}'
  pure $ Def name body

program :: Parser Program
program = do
  justWs
  defs <- many $ try $ ws opDef
  topLevel <- many $ ws op
  eof
  pure $ Program defs topLevel

parse :: String -> Either String Program
parse input =
  case runParser program () "" input of
    Left err -> Left $ show err
    Right p -> Right p