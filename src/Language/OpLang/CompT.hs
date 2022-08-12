module Language.OpLang.CompT(CompT(..)) where

import Control.Applicative(Alternative)
import Control.Monad(MonadPlus)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Reader(MonadReader, ReaderT(..))
import Control.Monad.Trans(MonadTrans(..))
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Writer.Strict(MonadWriter, WriterT(..))
import Data.Coerce(coerce)
import Data.Text(Text)

import Opts(Opts)

-- The "Compilation" Monad Transformer
newtype CompT m a =
  CompT { runCompT :: Opts -> m (Maybe a, [Text]) }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadReader Opts
    , MonadWriter [Text]
    , MonadIO
    )
    via ReaderT Opts (MaybeT (WriterT [Text] m))

instance MonadTrans CompT where
  lift = coerce . lift @(ReaderT Opts) . lift @MaybeT . lift @(WriterT [Text])
