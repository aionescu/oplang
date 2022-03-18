module Language.OpLang.Codegen(compile) where

import Control.Monad(unless)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(ask)
import Data.Char(ord)
import Data.Foldable(fold)
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Numeric(showHex)
import System.Directory(removeFile)
import System.FilePath(dropExtension)
import System.Process(system)
import Text.Builder(Builder)
import Text.Builder qualified as B

import Control.Monad.Comp(Comp)
import Data.Opts(Opts(..))
import Language.OpLang.Syntax(Id, Op(..), Program(..))

type CCode = Builder

showC :: Show a => a -> CCode
showC = B.string . show

cName :: Id -> CCode
cName n = "o" <> B.string (showHex (ord n) "")

programPrologue :: Word -> Word -> CCode
programPrologue stackSize tapeSize =
  "#include<stdio.h>\n#include<string.h>\n#define S "
  <> showC stackSize
  <> "\n#define T "
  <> showC tapeSize
  <> "\nchar s_[S],*s=s_;"

compileProto :: Id -> CCode
compileProto name = "void " <> cName name <> "();"

compileDef :: Id -> [Op] -> CCode
compileDef name body = "void " <> cName name <> "(){char t_[T],*t=t_;memset(t,0,T);" <> compileOps body <> "}"

compileMain :: [Op] -> CCode
compileMain body = "int main(){char t_[T],*t=t_;memset(t,0,T);" <> compileOps body <> "return 0;}"

compileOps :: [Op] -> CCode
compileOps = foldMap $ compileOp "t"

sign :: (Ord a, Num a) => a -> CCode
sign n
  | n < 0 = "-"
  | otherwise = "+"

repeatText :: Word -> Text -> CCode
repeatText n = B.text . T.concat . replicate (fromIntegral n)

compileOp :: CCode -> Op -> CCode
compileOp tape = \case
  Add n -> "*" <> tape <> sign n <> "=" <> showC (abs n) <> ";"
  Move n -> tape <> sign n <> "=" <> showC (abs n) <> ";"
  Set n -> "*" <> tape <> "=" <> showC n <> ";"
  Pop n -> "*" <> tape <> "=*(s-=" <> showC n <> ");"
  Push -> "*(s++)=*" <> tape <> ";"
  Peek -> "*" <> tape <> "=*(s-1);"
  WithOffset off op -> compileOp ("(" <> tape <> "+" <> showC off <> ")") op
  Loop ops -> "while(*t){" <> compileOps ops <> "}"
  Read -> "scanf(\"%c\"," <> tape <> ");"
  Write 1 -> "printf(\"%c\",*" <> tape <> ");"
  Write n -> "{char c=*" <> tape <> ";printf(\"" <> repeatText n "%c" <> "\"" <> repeatText n ",c" <> ");}"
  Call c -> cName c <> "();"

codegen :: Word -> Word -> Program -> Text
codegen stackSize tapeSize Program{..} =
  B.run
  $ programPrologue stackSize tapeSize
  <> fold (compileProto <$> M.keys opDefs)
  <> fold (M.mapWithKey compileDef opDefs)
  <> compileMain topLevel

cFile :: FilePath -> FilePath
cFile file = dropExtension file <> ".c"

compile :: Program -> Comp ()
compile p = do
  Opts{..} <- ask
  let cPath = cFile optsPath
  let code = codegen optsStackSize optsTapeSize p

  liftIO do
    T.writeFile cPath code
    system $ show optsCCPath <> " -o " <> show optsOutPath <> " " <> show cPath

    unless optsKeepCFile $
      removeFile cPath
