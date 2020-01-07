module Codegen.LLVM(compile) where

import AST(Dict)
import Opts(Opts(..))

compile :: Opts -> Dict -> IO ()
compile _ _ = putStrLn "LLVM backend not yet implemented."