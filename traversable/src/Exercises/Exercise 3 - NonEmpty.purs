module Exercises.NonEmpty where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Foldable (class Foldable
                     ,foldr
                     ,foldl
                     ,foldMap
                     )
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable
                        ,traverse
                        )

data NonEmpty a = NonEmpty a (Array a)

derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance nonEmptyShow :: Show a => Show (NonEmpty a) where
  show = genericShow

derive instance nonEmptyEq :: Eq a => Eq (NonEmpty a)

-- to do Traversable we need to have instances of Functor and Foldable

instance nonEmptyFunctor :: Functor NonEmpty where
  map f (NonEmpty x xs) = (NonEmpty (f x) (map f xs))

instance nonEmptyFoldable :: Foldable NonEmpty where
  --foldr :: forall (@f :: Type -> Type) (a :: Type) (b :: Type). Foldable f =>
  --          (a -> b -> b) -> b -> f a -> b
  foldr fabb acc (NonEmpty x xs) = x `fabb` foldr fabb acc xs
  --foldl :: forall (@f :: Type -> Type) (a :: Type) (b :: Type). Foldable f =>
  --          (b -> a -> b) -> b -> f a -> b
  foldl fbab acc (NonEmpty x xs) = foldl fbab (acc `fbab` x) xs

  --foldMap :: forall (@f :: Type -> Type) (a :: Type) (m :: Type). 
  --            Foldable f => Monoid m => (a -> m) -> f a -> m
  foldMap fam (NonEmpty a as) = (fam a) <> (foldMap fam as)

---- Traverseable's kind is (Type -> Type) -> Constraint
instance nonEmptyTraversable :: Traversable NonEmpty where
  --traverse :: forall (@t :: Type -> Type)
  --                   ( a :: Type )
  --                   ( b :: Type )
  --                   ( m :: Type -> Type) .
  --                      Traversable t => Applicative m =>
  --                        (a -> m b) -> t a -> m (t b)
  -- in this case
  --traverse :: forall (a :: Type) (b :: Type) (m :: Type -> Type).
  --              Applicative m => (a -> m b) -> NonEmpty a -> m (NonEmpty b)
  traverse famb (NonEmpty a as) = ado
    b  <- famb a
    bs <- traverse famb as
    in (NonEmpty b bs)

  -- TODO either use sequenceDefault, id, do it by hand,  or something
  sequence = traverse (\x -> x)
