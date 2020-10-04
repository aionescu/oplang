module Language.OpLang.Codegen.C(compile) where

import Data.Char(ord)
import Numeric(showHex)

import Control.Monad(unless)

import qualified Data.HashMap.Strict as HashMap

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Builder(Builder)
import qualified Text.Builder as B

import System.Directory(removeFile)
import System.FilePath(dropExtension)
import System.Process(system)

import Language.OpLang.AST(Op(..), Name, Body, Dict)
import Language.OpLang.Opts(Opts(..))

type CCode = Builder

showT :: Show a => a -> CCode
showT = B.string . show

cName :: Name -> CCode
cName Nothing = "main"
cName (Just name) = "o" <> B.string (showHex (ord name) "")

programPrologue :: Word -> Word -> CCode
programPrologue stackSize tapeSize =
  "#include<stdio.h>\n#include<string.h>\n#define S "
  <> showT stackSize
  <> "\n#define T "
  <> showT tapeSize
  <> "\nchar s_[S],*s=s_;"

compileProto :: Name -> Body -> CCode
compileProto name _ = "void " <> cName name <> "();"

compileDef :: Name -> Body -> CCode
compileDef name body = "void " <> cName name <> "(){char t_[T],*t;l:t=t_;memset(t,0,T);" <> compileOps name body <> "}"

compileMain :: Body -> CCode
compileMain body = "int main(){char t_[T],*t=t_;memset(t,0,T);" <> compileOps Nothing body <> "return 0;}" 

compileOps :: Name -> [Op] -> CCode
compileOps name ops = mconcat $ compileOp name "t" <$> ops

sign :: (Ord a, Num a) => a -> CCode
sign n
  | n < 0 = "-"
  | otherwise = "+"

repeatText :: Word -> Text -> CCode
repeatText n text = B.text $ T.concat $ replicate (fromIntegral n) text

compileOp :: Name -> CCode -> Op -> CCode
compileOp name tape = \case
  Add n -> "*" <> tape <> sign n <> "=" <> showT (abs n) <> ";"
  Move n -> tape <> sign n <> "=" <> showT (abs n) <> ";"
  Set n -> "*" <> tape <> "=" <> showT n <> ";"
  Pop n -> "*" <> tape <> "=*(s-=" <> showT n <> ");"
  Push -> "*(s++)=*" <> tape <> ";"
  Peek -> "*" <> tape <> "=*(s-1);"
  WithOffset off op -> compileOp name ("(" <> tape <> "+" <> showT off <> ")") op
  Loop ops -> "while(*t){" <> compileOps name ops <> "}"
  Read -> "scanf(\"%c\"," <> tape <> ");"
  Write 1 -> "printf(\"%c\",*" <> tape <> ");"
  Write n -> "{char c=*" <> tape <> ";printf(\"" <> repeatText n "%c" <> "\"" <> repeatText n ",c" <> ");}"
  OpCall c -> cName c <> "();"
  TailCall -> "goto l;"

codegen :: Word -> Word -> Dict -> Text
codegen stackSize tapeSize d =
  let d' = HashMap.delete Nothing d
  in
    B.run
    $ programPrologue stackSize tapeSize
    <> mconcat (HashMap.elems . HashMap.mapWithKey compileProto $ d')
    <> mconcat (HashMap.elems . HashMap.mapWithKey compileDef $ d')
    <> compileMain (d HashMap.! Nothing)

cFile :: String -> String
cFile file = dropExtension file <> ".c"

compile :: Opts -> Dict -> IO ()
compile Opts{..} d = do
  let cPath = cFile optsPath
  let code = codegen optsStackSize optsTapeSize d

  T.writeFile cPath code

  system ("cc -o " <> optsOutPath <> " " <> cPath)

  unless optsKeepCFile $
    removeFile cPath