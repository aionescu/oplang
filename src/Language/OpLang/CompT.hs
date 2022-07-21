module Language.OpLang.CompT(CompT, runCompT) where

import Control.Applicative(Alternative)
import Control.Monad(MonadPlus)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Reader(MonadReader, ReaderT(..))
import Control.Monad.Trans(MonadTrans(..))
import Control.Monad.Trans.Maybe(MaybeT (runMaybeT))
import Control.Monad.Writer.Strict(MonadWriter, WriterT, runWriterT)
import Data.Text(Text)
import Data.Tuple(swap)

import Opts

-- The "Compilation" Monad Transformer
newtype CompT m a =
  CompT { _runCompT :: ReaderT Opts (MaybeT (WriterT [Text] m)) a }
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
  lift = CompT . lift . lift . lift

runCompT :: Monad m => CompT m a -> Opts -> m ([Text], Maybe a)
runCompT (CompT m) opts = swap <$> runWriterT (runMaybeT $ runReaderT m opts)
