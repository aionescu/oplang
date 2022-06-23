module Language.OpLang.Validate(validate) where

import Control.Monad(guard)
import Control.Monad.Writer.Strict(tell)
import Data.Bifunctor(bimap)
import Data.Functor(($>))
import Data.List(intercalate)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T

import Comp
import Language.OpLang.IR

calledOps :: [Op] -> Set Id
calledOps = foldMap \case
  Call' op -> S.singleton op
  Loop' ops -> calledOps ops
  _ -> S.empty

enumerate :: Show a => [a] -> Text
enumerate l = T.pack $ intercalate ", " $ show <$> l

errUndefinedCalls :: Program Op -> Comp ()
errUndefinedCalls Program{..} = tell errors *> guard (null errors)
  where
    defined = M.keysSet opDefs

    undefinedInTopLevel = (Nothing, calledOps topLevel S.\\ defined)
    undefinedInDefs = bimap Just ((S.\\ defined) . calledOps) <$> M.toList opDefs

    fmt = maybe "top level" $ ("definition of " <>) . T.pack . show
    toMsg (name, ops) =
      "Error (in " <> fmt name <> "): Calls to undefined operators: " <> enumerate (S.toList ops)

    errors = toMsg <$> filter (not . S.null . snd) (undefinedInTopLevel : undefinedInDefs)

allUsedOps :: Map Id [Op] -> Set Id -> [Op] -> Set Id
allUsedOps defs seen ops
  | S.null used = seen
  | otherwise = foldMap (allUsedOps defs (seen <> used) . (defs M.!)) used
  where
    used = calledOps ops S.\\ seen

warnUnusedOps :: Program Op -> Comp (Program Op)
warnUnusedOps p@Program{..} =
  tell warning $> p { opDefs = usedDefs }
  where
    warning =
      [ "Warning: Unused operators: " <> enumerate (M.keys unusedDefs)
      | not $ M.null unusedDefs
      ]

    unusedDefs = opDefs M.\\ usedDefs
    usedDefs = M.restrictKeys opDefs usedOps
    usedOps = allUsedOps opDefs S.empty topLevel

validate :: Program Op -> Comp (Program Op)
validate p = errUndefinedCalls p *> warnUnusedOps p
