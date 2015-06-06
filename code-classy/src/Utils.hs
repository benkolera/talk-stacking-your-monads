{-# LANGUAGE NoImplicitPrelude     #-}
module Utils where

import BasePrelude

import Control.Monad.Error.Hoist ((<%!?>))
import Data.Validation (AccValidation(..))
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Monad.Except (MonadError,throwError)
import Control.Monad.Trans.Either (EitherT,eitherT)

throwAccValidation :: (Applicative m, MonadError e m) => (es -> e) -> AccValidation es a -> m a
throwAccValidation f (AccFailure es) = throwError (f es)
throwAccValidation _ (AccSuccess a)  = pure a

wrapException
  :: (Exception e, MonadError e' m,MonadIO m, Applicative m)
  => (e -> e')
  -> IO a
  -> m a
wrapException f a = do
  liftIO (catch (fmap Right a) (pure . Left . f)) <%!?> id

wrapExceptions
  :: (MonadError e m,MonadIO m, Applicative m)
  => IO a
  -> [Handler e]
  -> m a
wrapExceptions a hs =
  liftIO (catches (fmap Right a) handlers) <%!?> id
  where
    handlers = fmap (fmap Left) hs
