{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Main
( module Main
)
where

-- base
import qualified Control.Exception          as E
import           Control.Monad              (guard, (<=<))
import           Data.Char                  (isUpper, readLitChar)
import           Data.Foldable              (for_)
import           Data.Kind                  (Type)
import           Numeric                    (readDec, readSigned)
import           System.IO                  (hPutStrLn, stderr)

-- transformers
import qualified Control.Monad.Trans.Except as T
import qualified Control.Monad.Trans.State  as T

-- mtl
import qualified Control.Monad.Trans.Class  as MTL

-- fused-effects
import           Control.Algebra
import qualified Control.Effect.State       as FE

main :: IO ()
main = pure ()


parse :: ReadS a -> String -> Maybe a
parse parser input = case parser input of
  [(a, "")] -> Just a
  _         -> Nothing

match :: (a -> Bool) -> (a -> Maybe a)
match f a = do
  guard (f a)
  return a


parsePositiveDec :: String -> Maybe Int
parsePositiveDec = match (>= 0) <=< parse (readSigned readDec)

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
innocuous = hPutStrLn stderr $ replicate 10000 '✨'

-- ioAction :: IO ()
-- ioAction = E.throwTo fan glitter


-- concrete monads don’t compose; given monads m and n, m . n is not in general a monad.


-- monad transformers are brittle & static


-- Also, none of these give us any means to inspect or alter their behaviour. E.g. logging, statting, batching, concurrency…

writeCacheIO :: Show a => [a] -> IO ()
writeCacheIO things = do
  putStrLn $ "INFO: (writeCacheIO) starting write of " <> show nThings <> " things"

  for_ (zip [1..] things) $ \ (i, thing) ->
    writeFile ("cache/" <> show i) (show thing)

  putStrLn $ "INFO: (writeCacheIO) ending write of " <> show nThings <> " things"
  where
  nThings = length things


-- State

-- data State s (m :: Type -> Type) a where
--   Get ::      State s m s
--   Put :: s -> State s m ()

-- get :: Has (State s) sig m => m s
-- get = send Get

-- put :: Has (State s) sig m => s -> m ()
-- put s = send (Put s)


-- Laws

{-

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

-}


data Level = Info | Warning | Error
  deriving (Eq, Ord, Show)

data Logging (m :: Type -> Type) a where
  Log :: Level -> String -> Logging m ()
  Label :: String -> m a -> Logging m a

info :: Has Logging sig m => String -> m ()
info msg = send $ Log Info msg

warn :: Has Logging sig m => String -> m ()
warn msg = send $ Log Warning msg

err :: Has Logging sig m => String -> m ()
err msg = send $ Log Error msg


label :: Has Logging sig m => String -> m a -> m a
label name m = send $ Label name m

-- laws:
-- - atomicity
-- - thread-local ordering
-- - labelling


data Message = Message
  { context  :: [String]
  , level   :: Level
  , message :: String
  }
  deriving (Eq, Show)
