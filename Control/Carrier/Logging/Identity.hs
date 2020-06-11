{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Carrier.Logging.Identity
( module Control.Carrier.Logging.Identity
, module Control.Effect.Logging
) where

import           Control.Algebra
import           Control.Effect.Logging

newtype LoggingT m a = LoggingT { runLogging :: m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Logging :+: sig) (LoggingT m) where
  alg (L (Log _ _   k)) = k
  alg (L (Label _ m k)) = m >>= k
  alg (R other)         = LoggingT (alg (handleCoercible other))
