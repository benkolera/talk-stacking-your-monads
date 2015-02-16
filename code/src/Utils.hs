{-# LANGUAGE NoImplicitPrelude     #-}
module Utils where

import BasePrelude

import Data.Validation (AccValidation(..))
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Monad.Except (MonadError,throwError)
import Control.Monad.Trans.Either (EitherT,eitherT)

throwEither :: (Applicative m, MonadError e m) => Either e a -> m a
throwEither = either throwError pure

throwEitherT :: (Applicative m, MonadError e m) => EitherT e m a -> m a
throwEitherT = eitherT throwError pure

throwAccValidation :: (Applicative m, MonadError e m) => (es -> e) -> AccValidation es a -> m a
throwAccValidation f (AccFailure es) = throwError (f es)
throwAccValidation _ (AccSuccess a)  = pure a

wrapException
  :: (Exception e, MonadError e' m,MonadIO m, Applicative m)
  => (e -> e')
  -> IO a
  -> m a
wrapException f a = do
  liftIO (catch (fmap Right a) (pure . Left . f)) >>= throwEither
