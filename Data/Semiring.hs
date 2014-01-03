{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Data.Semiring where

import Data.Monoid
import Data.Function
import Numeric.IEEE

class (Monoid (SemiringPlus v), Monoid (SemiringProduct v)) => Semiring v where
  type SemiringPlus v :: *
  toPlus :: v -> SemiringPlus v
  fromPlus :: SemiringPlus v -> v
  
  type SemiringProduct v :: *
  toProduct :: v -> SemiringProduct v
  fromProduct :: SemiringProduct v -> v
  
  zero :: v
  zero = fromPlus mempty
  infixl 6 <+>
  (<+>) :: v -> v -> v
  (<+>) = ((fromPlus .).) ((<>) `on` toPlus)
  
  one :: v
  one = fromProduct mempty
  infixl 7 <*>
  (<*>) :: v -> v -> v
  (<*>) = ((fromProduct .).) ((<>) `on` toProduct)

instance Semiring Double where
  type SemiringPlus Double = Sum Double
  toPlus = Sum
  fromPlus = getSum
  type SemiringProduct Double = Product Double
  toProduct = Product
  fromProduct = getProduct


newtype Max a = Max { getMax :: a }
                deriving (Show, Read, Eq)

instance IEEE a => Monoid (Max a) where
  mempty = Max (-infinity)
  (Max x) `mappend` (Max y) = Max (max x y)


newtype Tropical a = Tropical { getTropical :: a }
                   deriving (Show, Read, Eq)

instance IEEE a => Semiring (Tropical a) where
  type SemiringPlus (Tropical a) = Max a
  toPlus (Tropical x) = Max x
  fromPlus (Max x) = Tropical x
  
  type SemiringProduct (Tropical a) = Sum a
  toProduct (Tropical x) = Sum x
  fromProduct (Sum x) = Tropical x
