module Language.OpLang.Parser(parse) where

import Control.Monad(when)
import Control.Monad.Reader(ask)
import Control.Monad.Trans(lift)
import Control.Monad.Writer(tell)
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

import Control.Monad.Comp(CompT)
import Language.OpLang.Syntax
import Opts(Opts(..))

type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

reserved :: Text
reserved = "+-<>,.;:[]{}"

intrinsic :: Parser Op
intrinsic =
  choice
  [ symbol "+" $> Incr
  , symbol "-" $> Decr
  , symbol "<" $> MoveL
  , symbol ">" $> MoveR
  , symbol "," $> Read'
  , symbol "." $> Write'
  , symbol ";" $> Pop'
  , symbol ":" $> Push'
  ] <?> "intrinsic operator"

block :: Text -> Text -> Parser [Op]
block b e = between (symbol b) (symbol e) $ many op

loop :: Parser Op
loop = Loop' <$> block "[" "]" <?> "loop"

custom :: Parser Char
custom = lexeme (satisfy $ not . (`T.elem` reserved)) <?> "custom operator"

op :: Parser Op
op =
  choice
  [ loop
  , intrinsic
  , Call' <$> custom
  ] <?> "operator"

def :: Parser (Id, [Op])
def = (,) <$> custom <*> block "{" "}" <?> "definition"

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

program :: Parser (Program Op)
program = Program <$> defs <*> (many op <?> "toplevel")

parse :: Text -> CompT IO (Program Op)
parse code = do
  Opts{..} <- ask
  case runParser (ws *> program <* eof) path code of
    Left e -> tell ["Parse error at " <> T.pack (errorBundlePretty e)] *> empty
    Right p -> when dumpAST (lift $ putStrLn $ "AST:\n" <> show p <> "\n") $> p
