{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Carrier.Logging.IO
( module Control.Carrier.Logging.IO
, module Control.Effect.Logging
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Logging
import           Control.Monad.IO.Class
import           Data.List              (intercalate)

newtype LoggingT m a = LoggingT { runLogging :: ReaderC [String] m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Logging :+: sig) (LoggingT m) where
  alg (L (Log lvl msg  k)) = do
    ctx <- LoggingT $ asks displayContext
    liftIO $ putStrLn $ displayLevel lvl <> ": " <> "(" <> ctx <> ")" <> msg
    k
  alg (L (Label name m k)) = do
    res <- LoggingT $ local (name:) (runLogging m)
    k res
  alg (R other)            = LoggingT (alg (R (handleCoercible other)))

displayContext :: [String] -> String
displayContext = intercalate "."
