-- Our Parse monad is not a perfect replacement for its earlier
-- counterpart. Because we are using Maybe instead of Either to represent a
-- result, we can't report any useful information if a parse fails.
--
-- Create an EitherT sometype monad transformer, and use it to implement a more
-- capable Parse monad that can report an error message if parsing fails.
--
-- Tip: If you like to explore the Haskell libraries for fun, you may have run
-- across an existing Monad instance for the Either type in the
-- Control.Monad.Error module. We suggest that you do not use that as a
-- guide. Its design is too restrictive: it turns Either String into a monad,
-- when you could use a type parameter instead of String.
--
-- Hint: if you follow this suggestion, you'll probably need to use the
-- FlexibleInstances language extension in your definition.


{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L


{-- From examples/examples/ch18/MaybeTParse.hs and modified --}
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

newtype Parse a = P {
      runP :: EitherT (State ParseState) a
      -- Added Functor and Applicative to deriving
    } deriving (Functor, Applicative, Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Either String a
evalParse m s = evalState (runEitherT (runP m)) (ParseState s 0)
{-- End of code from examples --}


{-- From examples/examples/ch18/MaybeT.hs and modified --}
newtype EitherT m a = EitherT {
      runEitherT :: m (Either String a)
    }

bindET :: (Monad m) => EitherT m a -> (a -> EitherT m b) -> EitherT m b
x `bindET` f = EitherT $ do
                 unwrapped <- runEitherT x
                 case unwrapped of
                   Left err -> return $ Left err
                   Right y -> runEitherT (f y)

failET :: (Monad m) => String -> EitherT m a
failET err = EitherT $ return $ Left err

instance (Monad m) => Monad (EitherT m) where
  -- Use 'pure' as suggested by the GHC instead of noncanonical 'return'
  -- definition (like the 'returnMT')
  return = pure
  (>>=) = bindET

instance (Monad m) => MonadFail (EitherT m) where
  fail s = failET s

instance MonadTrans EitherT where
    lift m = EitherT (Right `liftM` m)

instance (Functor m) => Functor (EitherT m) where
  fmap f x = EitherT $ fmap (fmap f) . runEitherT $ x

instance (Applicative m) => Applicative (EitherT m) where
  pure :: a -> EitherT m a
  pure x = EitherT $ pure $ Right x

  (<*>) :: EitherT m (a -> b) -> EitherT m a -> EitherT m b
  ff <*> fa = EitherT $ (fmap x (runEitherT ff)) <*> (runEitherT fa)
    where
      x :: Either String (a -> b) -> Either String a -> Either String b
      x f a = f <*> a

instance (MonadState s m) => MonadState s (EitherT m) where
  get = lift get
  put k = lift (put k)
{-- End of code from examples --}


identityParse :: a -> Parse a
identityParse x = P $ pure x

failParse :: a -> Parse a
failParse _ = P $ fail "Test error in fail"


-- ghci> :l 18_b_1.hs
-- [1 of 2] Compiling Main             ( 18_b_1.hs, interpreted )
-- Ok, one module loaded.

-- ghci> evalParse (identityParse 123) L.empty
-- Right 123

-- ghci> evalParse (identityParse "Test value") L.empty
-- Right "Test value"

-- ghci> evalParse (failParse "Test value") L.empty
-- Left "Test error in fail"
