module Comp where

import Control.Applicative(Alternative)
import Control.Monad(MonadPlus)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Reader(MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Maybe(MaybeT (runMaybeT))
import Control.Monad.Writer.Strict(MonadWriter, WriterT, runWriterT)
import Data.Text(Text)
import Data.Tuple(swap)

import Opts

-- The "Compilation" Monad
newtype Comp a =
  Comp { runComp' :: ReaderT Opts (MaybeT (WriterT [Text] IO)) a }
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

runComp :: Comp a -> Opts -> IO ([Text], Maybe a)
runComp (Comp comp) opts = swap <$> runWriterT (runMaybeT $ runReaderT comp opts)
