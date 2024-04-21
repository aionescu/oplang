module Language.OpLang.Validation(validate) where

import Control.Monad(unless)
import Control.Monad.Chronicle(MonadChronicle(..))
import Data.Bifunctor(bimap)
import Data.Foldable(foldMap')
import Data.Functor((<&>))
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Maybe(fromMaybe)
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T

import Language.OpLang.Syntax

calledOps :: [Op] -> Set Name
calledOps = foldMap' \case
  Call' op -> S.singleton op
  Loop' ops -> calledOps ops
  _ -> S.empty

undefinedCalls :: Program Op -> [Text]
undefinedCalls Program{..} =
  toMsgs =<< filter (not . S.null . snd) (undefinedInTopLevel : undefinedInDefs)
  where
    defined = M.keysSet opDefs

    undefinedInTopLevel = (Nothing, calledOps topLevel S.\\ defined)
    undefinedInDefs = bimap Just ((S.\\ defined) . calledOps) <$> M.toList opDefs

    fmt = maybe "top level" $ ("definition of " <>) . T.pack . show
    toMsgs (name, ops) = S.toList ops <&> \op ->
      "Error: In " <> fmt name <> ": Call to undefined operator " <> T.pack (show op) <> "."

allUsedOps :: Map Name [Op] -> Set Name -> [Op] -> Set Name
allUsedOps defs seen ops
  | S.null used = seen
  | otherwise = foldMap' ((allUsedOps defs (seen <> used)) . fromMaybe [] . (defs M.!?)) used
  where
    used = calledOps ops S.\\ seen

removeUnusedOps :: Program Op -> ([Text], Program Op)
removeUnusedOps p@Program{..} = (warnings, p{opDefs = usedDefs})
  where
    usedOps = allUsedOps opDefs S.empty topLevel
    unusedDefs = M.withoutKeys opDefs usedOps
    usedDefs = M.restrictKeys opDefs usedOps

    warnings = M.keys unusedDefs <&> \op ->
      "Warning: Unused operator " <> T.pack (show op) <> "."

validate :: MonadChronicle [Text] m => Bool -> Program Op -> m (Program Op)
validate noWarn p = do
  let (warnings, p') = removeUnusedOps p
  unless noWarn $ dictate warnings

  case undefinedCalls p of
    [] -> pure p'
    errs -> confess errs
