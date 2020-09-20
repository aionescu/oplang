module Parser(Parser.parse) where

import Data.Text(Text)
import qualified Data.Text as T

import Text.Parsec((<|>), anyChar, between, char, choice, endOfLine, eof, many, manyTill, noneOf, Parsec, runParser, space, skipMany, try)

import AST(Op(..), Def, DefList, incr, decr, movl, movr, pop, write)

type Parser = Parsec Text ()

skip :: Parser a -> Parser ()
skip p = do
  p
  pure ()

comment :: Parser ()
comment =
  char '#'
  *> skip (manyTill anyChar (skip endOfLine <|> eof))

justWs :: Parser ()
justWs = skipMany (skip space <|> comment)

ws :: Parser a -> Parser a
ws p = p <* justWs

intrinsics :: String
intrinsics = "+-<>,.;:"

reserved :: String
reserved = intrinsics ++ "[]{}# \t\r\n"

intrinsic :: Parser Op
intrinsic = choice [incr', decr', movl', movr', read', write', pop', push']
  where
    incr' = incr <$ char '+'
    decr' = decr <$ char '-'
    movl' = movl <$ char '<'
    movr' = movr <$ char '>'
    read' = Read <$ char ','
    write' = write <$ char '.'
    pop' = pop <$ char ';'
    push' = Push <$ char ':'

loop :: Parser Op
loop = Loop <$> between (ws $ char '[') (char ']') (many $ ws op)

custom :: Parser Char
custom = noneOf reserved

op :: Parser Op
op = choice [intrinsic, loop, (OpCall . Just) <$> custom]

opDef :: Parser Def
opDef = do
  name <- ws custom
  ws $ char '{'
  body <- many $ ws op
  char '}'
  pure $ (Just name, body)

program :: Parser DefList
program = do
  justWs
  defs <- many $ try $ ws opDef
  topBody <- many $ ws op
  let topLevel = (Nothing, topBody)
  eof
  pure (topLevel : defs)

parse :: Text -> Either Text DefList
parse input =
  case runParser program () "" input of
    Left err -> Left $ T.pack $ show err
    Right p -> Right p