{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import           Control.Carrier.Logging.Identity as Identity

-- base
import           Control.Monad                    ((<=<))
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           GHC.Generics                     (Generic1)
import           System.IO                        (hPutStr, hPutStrLn, stderr)

-- transformers
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except       as T
import qualified Control.Monad.Trans.State        as T

-- fused-effects
import           Control.Algebra
import qualified Control.Carrier.Error.Either     as FE
import qualified Control.Carrier.State.Strict     as FE

main :: IO ()
main = return ()


-- outline & code:
-- https://github.com/robrix/languages-all-the-way-down
-- https://github.com/robrix/starlight

-- me:
-- https://twitter.com/rob_rix


double, incr, twoNPlusOne :: Num a => a -> a

double n = n * 2

incr n = n + 1

twoNPlusOne = incr . double


copyFileToStderr :: FilePath -> IO ()
copyFileToStderr = hPutStr stderr <=< readFile



{-
Contrast:

(.)   ::            (b ->   c) -> (a ->   b) -> (a ->   c)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)

f . g     (for pure functions f and g)
is analogous to
f <=< g   (for monadic functions f and g)

Aside:

($)   ::            (a ->   b) ->   a ->   b
(=<<) :: Monad m => (a -> m b) -> m a -> m b

f $ x   (for pure function f and value x)
is analogous to
f =<< x (for monadic function f and computation x)
-}


-- IO actions give us no guarantees about what they can do:

innocuous :: IO ()
innocuous = hPutStrLn stderr $ replicate 10000 '✨'


-- monad transformers are brittle & static

stateOnExcept :: T.StateT Int (T.ExceptT String Identity) ()
stateOnExcept = do
  v <- T.get
  lift $ T.throwE (show v)

exceptOnState :: T.ExceptT String (T.StateT Int Identity) ()
exceptOnState = do
  v <- lift T.get
  T.throwE (show v)

-- can’t use >> to combine these


-- contrast:
errorAndState
  :: ( Has (FE.Error String) sig m
     , Has (FE.State Int) sig m
     )
  => m ()
errorAndState = do
  v <- FE.get
  FE.throwError (show (v :: Int))


-- Example: logging

logIO :: Level -> String -> IO ()
logIO level message = hPutStrLn stderr $ displayLevel level <> ": " <> message

infoIO, warnIO, errIO :: String -> IO ()

infoIO = logIO Info
warnIO = logIO Warning
errIO  = logIO Error

labelIO :: String -> IO a -> IO a
labelIO name m = do
  hPutStrLn stderr $ "NOTE: entering " <> name
  a <- m
  hPutStrLn stderr $ "NOTE: exited " <> name
  return a


-- Laws

{-

State:

get :: Has (FE.State s) sig m =>      m s
put :: Has (FE.State s) sig m => s -> m ()


aside: functor law:
  fmap f . fmap g = fmap (f . g)


get law:
  runState a (get >>= k)  = runState a (k a)
put law:
  runState a (put b >> m) = runState b m

might have expected a law relating get/put, directly stating the property we want to hold:
  put a >> get = return a

but this is derivable from the above. key: above laws relate operations to some handler with approximately the following type:
  runState :: s -> m a -> n (s, a) -- abstract, and approximate; m determines both s and n

we can think of this as meaning that the laws are parameterized by this handler, and so specific handlers would instantiate the type variables:
  runState :: s -> StateC s n a -> n (s, a) -- instantiating m to fused-effects’ StateC, i.e. m ~ StateC s n
  runState :: s -> StateC s Identity a -> Identity (s, a) -- instantiating n to Identity
this handler is itself also subject to laws governing return and >>=:
  runState s (return a) = return (s, a)
  runState s (m >>= k) = runState s m >>= \ (s', a) -> runState s' (k a)
this is essential for the following reasoning but is generally left implicit

put law
  runState a (put b >> m) = runState b m
instantiate m with get (and a continuation k, to show our work):
  runState a (put b >> get >>= k) = runState b (get >>= k)
simplify rhs with get law:
  runState a (put b >> get >>= k) = runState b (k b)
instantiate k with return:
  runState a (put b >> get >>= return) = runState b (return b)
simplify lhs using monad law:
  runState a (put b >> get) = runState b (return b)


Error:

throwError law:
  runError (throwError e >>= k) === runError (throwError e)
catchError law:
  runError (throwError e `catchError` f) = runError (f e)

-}


-- Glitter

newtype Glitter m k = Glitter (m k)
  deriving (Generic1)

instance HFunctor Glitter
instance Effect   Glitter

glitter :: Has Glitter sig m => m ()
glitter = send $ Glitter (pure ())


newtype GlitterT m a = GlitterT { runGlitter :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Glitter :+: sig) (GlitterT m) where
  alg (L (Glitter k)) = do
    liftIO . hPutStrLn stderr $ replicate 10000 '✨'
    k
  alg (R other)       = GlitterT (alg (handleCoercible other))
