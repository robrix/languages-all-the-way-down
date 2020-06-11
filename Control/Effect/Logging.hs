{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Control.Effect.Logging
( module Control.Effect.Logging
) where

import Control.Algebra
import Data.Kind (Type)

data Level = Info | Warning | Error
  deriving (Eq, Ord, Show)

data Logging (m :: Type -> Type) a where
  Log :: Level -> String -> Logging m ()
  Label :: String -> m a -> Logging m a

log' :: Has Logging sig m => Level -> String -> m ()
log' level msg = send $ Log level msg

info, warn, err :: Has Logging sig m => String -> m ()

info = log' Info
warn = log' Warning
err  = log' Error


label :: Has Logging sig m => String -> m a -> m a
label name m = send $ Label name m
