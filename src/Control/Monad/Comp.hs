module Control.Monad.Comp(CompT(..), runCompT) where

import Control.Applicative(Alternative)
import Control.Monad(MonadPlus)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Reader(MonadReader, ReaderT(..))
import Control.Monad.Trans(MonadTrans(..))
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Writer.Strict(MonadWriter, WriterT(..))
import Data.Text(Text)

import Opts(Opts)

-- The "Compilation" Monad Transformer
newtype CompT m a =
  CompT (ReaderT Opts (MaybeT (WriterT [Text] m)) a)
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadReader Opts
    , MonadWriter [Text]
    , MonadIO
    )

instance MonadTrans CompT where
  lift :: Monad m => m a -> CompT m a
  lift = CompT . lift . lift . lift

runCompT :: CompT m a -> Opts -> m (Maybe a, [Text])
runCompT (CompT m) = runWriterT . runMaybeT . runReaderT m
