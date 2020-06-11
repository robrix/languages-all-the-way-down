{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Logging where

import Control.Algebra
import Data.Kind (Type)

data Level = Info | Warning | Error
  deriving (Eq, Ord, Show)

displayLevel :: Level -> String
displayLevel level = case level of
  Info    -> "INFO"
  Warning -> "WARN"
  Error   -> "ERROR"


data Logging (m :: Type -> Type) k
  = Log Level String (m k)
  | forall a . Label String (m a) (a -> m k)

instance HFunctor Logging where
  hmap f (Log lvl msg  k) = Log lvl msg (f k)
  hmap f (Label name m k) = Label name (f m) (f . k)

log' :: Has Logging sig m => Level -> String -> m ()
log' level msg = send $ Log level msg (pure ())

info, warn, err :: Has Logging sig m => String -> m ()

info = log' Info
warn = log' Warning
err  = log' Error


label :: Has Logging sig m => String -> m a -> m a
label name m = send $ Label name m pure
