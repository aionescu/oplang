module Language.OpLang.Parser(parse) where

import Control.Monad.Reader(asks)
import Control.Monad.Writer.Strict(tell)
import Data.Functor(($>))
import Data.List(intercalate)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char(space1)
import Text.Megaparsec.Char.Lexer qualified as L

import Comp(Comp)
import Opts(optsPath)
import Language.OpLang.IR(Program(..), Op(..), Id, NoOff)

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

reserved :: [Char]
reserved = "+-<>,.;:[]{}"

intrinsic :: Parser (Op NoOff)
intrinsic =
  choice
  [ symbol "+" $> Add () 1
  , symbol "-" $> Add () -1
  , symbol "<" $> Move -1
  , symbol ">" $> Move 1
  , symbol "," $> Read ()
  , symbol "." $> Write () 1
  , symbol ";" $> Pop () 1
  , symbol ":" $> Push ()
  ]
  <?> "intrinsic operator"

block :: Text -> Text -> Parser [Op NoOff]
block b e = between (symbol b) (symbol e) $ many op

loop :: Parser (Op NoOff)
loop = Loop <$> block "[" "]" <?> "loop"

custom :: Parser Char
custom = lexeme (satisfy (`notElem` reserved) <?> "custom operator")

op :: Parser (Op NoOff)
op = choice [try loop, intrinsic, Call <$> custom] <?> "operator"

def :: Parser (Id, [Op NoOff])
def = (,) <$> lexeme custom <*> block "{" "}" <?> "definition"

defs :: Parser (Map Id [Op NoOff])
defs = many (try def) >>= toMap
  where
    toMap ds
      | unique = pure $ M.fromList ds
      | otherwise = fail $ "Duplicate definition of operators: " <> intercalate ", " (show <$> S.toList set)
      where
        ids = fst <$> ds
        set = S.fromList ids
        unique = S.size set == length ids

program :: Parser (Program NoOff)
program = Program <$> defs <*> (many op <?> "toplevel")

programFull :: Parser (Program NoOff)
programFull = ws *> program <* eof

parse :: Text -> Comp (Program NoOff)
parse code = do
  file <- asks optsPath

  case runParser programFull file code of
    Left e -> tell [T.pack $ errorBundlePretty e] *> empty
    Right p -> pure p
