{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Carrier.Logging.Tree
( module Control.Carrier.Logging.Tree
, module Control.Effect.Logging
) where

import           Control.Algebra
import           Control.Carrier.Writer.Strict
import           Control.Effect.Logging
import           Control.Monad.IO.Class

runLogging :: Monad m => LoggingT m a -> m (Span, a)
runLogging (LoggingT m) = do
  (children, (messages, a)) <- runWriter (runWriter m)
  return (Span "" messages children, a)

newtype LoggingT m a = LoggingT (WriterC [Message] (WriterC [Span] m) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, Effect sig, MonadIO m) => Algebra (Logging :+: sig) (LoggingT m) where
  alg (L (Log lvl msg  k)) = do
    LoggingT $ tell [ Message lvl msg ]
    k
  alg (L (Label name (LoggingT m) k)) = do
    (messages, (children, res)) <- LoggingT $ censor @[Message] (const []) (listen (censor @[Span] (const []) (listen m)))
    LoggingT $ tell [ Span name messages children ]
    k res
  alg (R other)            = LoggingT (alg (R (R (handleCoercible other))))


data Span = Span
  { name     :: String
  , messages :: [Message]
  , children :: [Span]
  }
  deriving (Eq, Show)

data Message = Message
  { level   :: Level
  , message :: String
  }
  deriving (Eq, Show)
