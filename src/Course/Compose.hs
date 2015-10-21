{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

runCompose :: Compose f g a -> (f (g a))
runCompose (Compose x) = x

inComp :: (f (g a) -> f (g b)) -> Compose f g a -> Compose f g b
inComp f = Compose . f . runCompose

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
    (<$>) = inComp . (<$>) . (<$>)
            
instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) = inComp . (<*>) . (<$>)(<*>) . runCompose

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "Monads are not closed under composition, hence the need for Monad Transformers."


