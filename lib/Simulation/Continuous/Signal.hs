{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, IncoherentInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Simulation.Continuous.Signal (
  Signal(..)
, Integrable(..)
, Extractable(..)
, EU1
, RK4
, Maybe(..)
, maybe
) where

import NumericPrelude

import qualified Algebra.Additive as Additive
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Module as Module
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.VectorSpace as VectorSpace


class Signal s where
  liftS0 :: a -> s a
  applyS :: s (a -> b) -> s a -> s b

  liftS1 :: (a -> b) -> (s a -> s b)
  liftS1 f = \x -> liftS0 f `applyS` x
  liftS2 :: (a -> b -> c) -> (s a -> s b -> s c)
  liftS2 f = \x y -> liftS1 f x `applyS` y
  liftS3 :: (a -> b -> c -> d) -> (s a -> s b -> s c -> s d)
  liftS3 f = \x y z -> liftS2 f x y `applyS` z
  liftS4 :: (a -> b -> c -> d -> e) -> (s a -> s b -> s c -> s d -> s e)
  liftS4 f = \x y z w -> liftS3 f x y z `applyS` w

class Signal s => Integrable s where
  integrate :: VectorSpace.C t a => t -> (s a) -> a -> (s a, a)

class Signal s => Extractable s where
  extractS :: s a -> a

instance (Signal s, Additive.C a) => Additive.C (s a) where
  {-# INLINE zero #-}
  {-# INLINE negate #-}
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  zero = liftS0 zero
  negate = liftS1 negate
  (+) = liftS2 (+)
  (-) = liftS2 (-)

instance (Signal s, Ring.C a) => Ring.C (s a) where
  {-# INLINE (*) #-}
  {-# INLINE one #-}
  {-# INLINE fromInteger #-}
  (*) = liftS2 (*)
  one = liftS0 one
  fromInteger = liftS0 . fromInteger

instance (Signal s, Field.C a) => Field.C (s a) where
  {-# INLINE (/) #-}
  {-# INLINE recip #-}
  {-# INLINE fromRational' #-}
  {-# INLINE (^-) #-}
  (/) = liftS2 (/)
  recip = liftS1 recip
  fromRational' = liftS0 . fromRational'
  (^-) = \x p -> liftS1 (^- p) x

instance (Signal s, Module.C a v) => Module.C (s a) (s v) where
  {-# INLINE (*>) #-}
  (*>) = liftS2 (*>)

instance (Signal s, Algebraic.C a) => Algebraic.C (s a) where
  {-# INLINE sqrt #-}
  {-# INLINE root #-}
  {-# INLINE (^/) #-}
  sqrt = liftS1 sqrt
  root = \x -> liftS1 (Algebraic.root x)
  (^/) = \x p -> liftS1 (^/ p) x

instance (Signal s, Transcendental.C a) => Transcendental.C (s a) where
  pi = liftS0 pi
  exp = liftS1 exp
  log = liftS1 log
  logBase = liftS2 logBase
  (**) = liftS2 (**)
  sin = liftS1 sin
  tan = liftS1 tan
  cos = liftS1 cos
  asin = liftS1 asin
  atan = liftS1 atan
  acos = liftS1 acos
  sinh = liftS1 sinh
  tanh = liftS1 tanh
  cosh = liftS1 cosh
  asinh = liftS1 asinh
  atanh = liftS1 atanh
  acosh = liftS1 acosh

instance Signal Maybe where
  {-# INLINE liftS0 #-}
  liftS0 x = Just x
  {-# INLINE applyS #-}
  applyS (Just f) (Just x) = Just (f x)
  applyS _ _ = Nothing

newtype EU1 v = EU1 v deriving (Show, Read, Eq)

instance Signal EU1 where
  {-# INLINE liftS0 #-}
  liftS0 x = EU1 x
  {-# INLINE applyS #-}
  applyS (EU1 f) (EU1 x) = EU1 (f x)

instance Integrable EU1 where
  {-# INLINE integrate #-}
  integrate = \dt (EU1 x') x0 -> let x = x0 + dt *> x' in (EU1 x0, x)

instance Extractable EU1 where
  extractS = \(EU1 x) -> x

data RK4 v = RK4 !v v v v deriving (Show, Read, Eq)

instance Signal RK4 where
  {-# INLINE liftS0 #-}
  liftS0 x = RK4 x x x x
  {-# INLINE applyS #-}
  applyS (RK4 f1 f2 f3 f4) (RK4 x1 x2 x3 x4) = RK4 (f1 x1) (f2 x2) (f3 x3) (f4 x4)

  {-# INLINE liftS1 #-}
  liftS1 f = \(RK4 v1 v2 v3 v4) -> RK4 (f v1) (f v2) (f v3) (f v4)
  {-# INLINE liftS2 #-}
  liftS2 f = \(RK4 x1 x2 x3 x4) (RK4 y1 y2 y3 y4) -> RK4 (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4)

instance Integrable RK4 where
  {-# INLINE integrate #-}
  integrate = let two = fromRational 2
                  six = fromRational 6
              in \dt (~(RK4 x1' x2' x3' x4')) x0 ->
              (RK4 x0 (x0 + (dt / two) *> x1') (x0 + (dt / two) *> x2') (x0 + dt *> x3'),
               x0 + (dt / six) *> (x1' + two *> x2' + two *> x3' + x4'))

instance Extractable RK4 where
  extractS = \(RK4 x _ _ _) -> x
