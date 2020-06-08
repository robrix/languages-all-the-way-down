{-# LANGUAGE RankNTypes #-}
module Main where

-- base
import qualified Control.Exception as E
import           Data.Foldable (for_)
import           System.IO (hPutStrLn, stderr)

-- transformers
import qualified Control.Monad.Trans.Except as T
import qualified Control.Monad.Trans.State as T

-- mtl
import qualified Control.Monad.Trans.Class as MTL

-- fused-effects
import qualified Control.Algebra as FE
import qualified Control.Effect.State as FE

main :: IO ()
main = pure ()


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
f =<< x (for pure function f and computation x)
-}


-- Effects ~= “side effects”; “special” control flow alongside the normal behaviours

-- pure function:                 a ->   b
-- effectful function: Monad m => a -> m b

-- pure functions are analogous to a -> Identity b, i.e. “effectful” functions with no extra behaviour provided by the resulting context


-- IO actions give us no guarantees about what they can do:

innocuous :: IO ()
innocuous = hPutStrLn stderr $ repeat '✨'

-- ioAction :: IO ()
-- ioAction = E.throwTo fan glitter


-- concrete monads don’t compose; given monads m and n, m . n is not in general a monad.


-- monad transformers are brittle & static


-- Also, none of these give us any means to inspect or alter their behaviour. E.g. logging, statting, batching, concurrency…

writeCacheIO :: Show a => [a] -> IO ()
writeCacheIO things = do
  putStrLn $ "starting write of " <> show nThings <> " things"

  for_ things $ \ thing ->
    appendFile "cache" (show thing)

  putStrLn $ "ending write of " <> show nThings <> " things"
  where
  nThings = length things

-- how do we test this?
-- what if we want to run tests concurrently?
-- what if we want to time it?
-- what about if we want to configure where the cache gets written?
-- or how?
-- do we really want to complicate the business logic with details of logging?
-- how do we batch the writes to the file?
-- if this is in a library, how do we ensure nobody else writes to our file?
-- if we’re given an action in IO, what confidence do we have that it isn’t going to dump glitter into a fan?

-- pros/cons…


-- get :: MTL.MonadState   s      m => m s
-- get :: FE.Has (FE.State s) sig m => m s
-- put :: MTL.MonadState   s      m => s -> m ()
-- put :: FE.Has (FE.State s) sig m => s -> m ()

-- throwError :: MTL.MonadError   e      m => e -> m a
-- throwError :: FE.Has (FE.Error e) sig m => e -> m a
-- catchError :: MTL.MonadError   e      m => m a -> (e -> m a) -> m a
-- catchError :: FE.Has (FE.Error e) sig m => m a -> (e -> m a) -> m a

-- key features:
-- - abstraction; computations occur in some type abstracted over the implementation of the effects
-- - composition; we can seamlessly introduce and handle effects, lift operations into supporting contexts, etc.


-- fused-effects defines interpreters using Algebra instances; handler functions select a specific type & thus its Algebra


-- Effects, more formally
