module Language.OpLang.Codegen.LLVM(compile) where

import Language.OpLang.AST(Dict)
import Language.OpLang.Opts(Opts(..))

compile :: Opts -> Dict -> IO ()
compile _ _ = putStrLn "LLVM backend not yet implemented."
