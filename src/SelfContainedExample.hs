{-# LANGUAGE NoImplicitPrelude, BangPatterns, ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude, Arrows, IncoherentInstances, Rank2Types #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
import NumericPrelude
import Data.List (transpose)
import Control.Arrow
import Control.Applicative (Applicative, pure, (<*>), liftA, liftA2)
import qualified Control.Category as Cat

import qualified Algebra.Additive as Additive
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Module as Module
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.VectorSpace as VectorSpace

-- Arrow-based streams are the building blocks for simulation

data Continuous t a b = forall s. Continuous !s (t -> a -> s -> (b, s))

simStep :: t -> a -> (Continuous t a b) -> (b, Continuous t a b)
simStep dt a (Continuous s f) = let !(b, s') = f dt a s in (b, Continuous s' f)

simTrace :: t -> [a] -> (Continuous t a b) -> [b]
simTrace dt (x:xs) f = let !(y, f') = simStep dt x f in y `seq` (y : simTrace dt xs f')
simTrace _ [] _ = []

instance Cat.Category (Continuous t) where
  id = Continuous () (\_ x _ -> (x, ()))
  Continuous sg g . Continuous sf f = Continuous (sg, sf) $ \dt x (!sg, !sf) ->
    let !(y, sf') = f dt x sf
        !(z, sg') = g dt y sg
    in (z, (sg', sf'))

instance Arrow (Continuous t) where
  arr f = Continuous () (\_ x _ -> (f x, ()))
  
  first (Continuous sf f) = Continuous sf $ \dt (x, r) !s ->
    let !(y, s') = f dt x s
    in ((y, r), s')

  second (Continuous sf f) = Continuous sf $ \dt (r, x) !s ->
    let !(y, s') = f dt x s
    in ((r, y), s')

  (Continuous sf f) *** (Continuous sg g) = Continuous (sf, sg) $ \dt (xf, xg) (!sf, !sg) ->
    let !(yf, sf') = f dt xf sf
        !(yg, sg') = g dt xg sg
    in ((yf, yg), (sf', sg'))

  (Continuous sf f) &&& (Continuous sg g) = Continuous (sf, sg) $ \dt x (!sf, !sg) ->
    let !(yf, sf') = f dt x sf
        !(yg, sg') = g dt x sg
    in ((yf, yg), (sf', sg'))

instance ArrowLoop (Continuous t) where
  loop (Continuous s f) = Continuous s $ \dt x !s ->
    let (!(y, r), s') = f dt (x, r) s
    in (y, s')


-- Signals are datatypes to represent current signal value
-- with some extra possibilities (e.g. first-second order derivatives)

class Applicative s => Signal s

-- Signals are the thing, that can be integrated
class Signal s => Integrable s where
  integrate :: VectorSpace.C t a => t -> (s a) -> a -> (s a, a)

-- Extract current value of the signal
class Signal s => Extractable s where
  extractS :: s a -> a

instance (Signal s, Additive.C a) => Additive.C (s a) where
  zero = pure zero
  negate = liftA negate
  (+) = liftA2 (+)
  (-) = liftA2 (-)

instance (Signal s, Ring.C a) => Ring.C (s a) where
  (*) = liftA2 (*)
  one = pure one
  fromInteger = pure . fromInteger

instance (Signal s, Field.C a) => Field.C (s a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational' = pure . fromRational'
  (^-) x p = liftA (^- p) x

instance (Signal s, Module.C a v) => Module.C (s a) (s v) where
  (*>) = liftA2 (*>)

instance (Signal s, Algebraic.C a) => Algebraic.C (s a) where
  sqrt = liftA sqrt
  root x = liftA (Algebraic.root x)
  (^/) x p = liftA (^/ p) x

instance (Signal s, Transcendental.C a) => Transcendental.C (s a) where
  pi = pure pi
  exp = liftA exp
  log = liftA log
  logBase = liftA2 logBase
  (**) = liftA2 (**)
  sin = liftA sin
  tan = liftA tan
  cos = liftA cos
  asin = liftA asin
  atan = liftA atan
  acos = liftA acos
  sinh = liftA sinh
  tanh = liftA tanh
  cosh = liftA cosh
  asinh = liftA asinh
  atanh = liftA atanh
  acosh = liftA acosh

-- Signal data with Euler method integration, and instances for it.

newtype EU1 v = EU1 v deriving (Show, Read, Eq)

instance Functor EU1 where
  fmap f (EU1 x) = EU1 (f x)

instance Applicative EU1 where
  pure x = EU1 x
  (EU1 f) <*> (EU1 x) = EU1 (f x)
  
instance Signal EU1

instance Integrable EU1 where
  integrate dt (EU1 x') x0 = let x = x0 + dt *> x' in (EU1 x0, x)

instance Extractable EU1 where
  extractS (EU1 x) = x

-- Signal data with implicit Euler method.

newtype IEU1 v = IEU1 [v] deriving (Show, Read, Eq, Functor, Applicative)

instance Signal IEU1

instance Integrable IEU1 where
  integrate dt (IEU1 xs') x0 =
    let xs = x0 : fmap (\x' -> x0 + dt *> x') xs' in (IEU1 xs, xs !! 2)

instance Extractable IEU1 where
  extractS (IEU1 xs) = head xs

-- Signal data with implicit Euler method.

newtype IMEU1 v = IMEU1 [v] deriving (Show, Read, Eq, Functor, Applicative)

instance Signal IMEU1

instance Integrable IMEU1 where
  integrate dt (IMEU1 xs') x0 =
    let xs = x0 : fmap (\x' -> x0 + (dt / fromRational' 2) *> x') xs'
    in (IMEU1 xs, x0 + dt *> (xs' !! 3))

instance Extractable IMEU1 where
  extractS (IMEU1 xs) = head xs

-- Signal data with Runge-Kutta integration, with instances.

data RK4 v = RK4 !v v v v deriving (Show, Read, Eq)

instance Functor RK4 where
  fmap f (RK4 x1 x2 x3 x4) = RK4 (f x1) (f x2) (f x3) (f x4)

instance Applicative RK4 where
  pure x = RK4 x x x x
  (RK4 f1 f2 f3 f4) <*> (RK4 x1 x2 x3 x4) = RK4 (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance Signal RK4

instance Integrable RK4 where
  integrate = let two = fromRational 2
                  six = fromRational 6
              in \dt (~(RK4 x1' x2' x3' x4')) x0 ->
              (RK4 x0 (x0 + (dt / two) *> x1') (x0 + (dt / two) *> x2') (x0 + dt *> x3'),
               x0 + (dt / six) *> (x1' + two *> x2' + two *> x3' + x4'))

instance Extractable RK4 where
  extractS = \(RK4 x _ _ _) -> x


-- Just a wrapper to make an arrow from "integrate"

integrator :: (Integrable s, VectorSpace.C t a) => a -> Continuous t (s a) (s a)
integrator x0 = Continuous x0 integrate

-- Simple sine wave as an example

sine :: (Signal s, Integrable s) => Continuous Double () (s Double)
sine = proc _ -> do
  rec x <- integrator 0.0 -< y
      y <- integrator 1.0 -< liftA negate x
  returnA -< x

sys1 :: (Signal s, Integrable s) => (Double, Double) -> Continuous Double () (s (Double, Double))
sys1 (u0, v0) = proc _ -> do
  rec u <- integrator u0 -< liftA2 (*) u (liftA2 (-) v (pure 2))
      v <- integrator v0 -< liftA2 (*) v (liftA2 (-) (pure 1) u)
  returnA -< liftA2 (,) u v

{-
vdp :: (Signal s, Integrable s) => Continuous Double (s ()) (s Double)
vdp = proc _ -> do
  rec x <- integratorS 0.0 -< x - liftA (/ 3) (x ^ 3) - y
      y <- integratorS 1.0 -< x
  returnA -< x
-}

-- Run an arrow

runIt :: forall a. Double -> Int -> (forall s. (Signal s, Integrable s) => Continuous Double () (s a)) -> [(Double, [a])]
runIt step n block =
  let input = repeat ()
      outputRK = simTrace step input block :: [RK4 a]
      outputEU = simTrace step input block :: [EU1 a]
      outputIEU = simTrace step input block :: [IEU1 a]
      outputIMEU = simTrace step input block :: [IMEU1 a]
      times = map (* step) [0, 1 ..] :: [Double]
  in take (succ n) $ zip times $ transpose [(map extractS outputEU), (map extractS outputRK), (map extractS outputIEU), (map extractS outputIMEU)]

-- Show the results of the run

showIt1 :: [(Double, [Double])] -> String
showIt1 = unlines . map (\(t, xs) -> unwords . map show $ (t : xs))

showIt2 :: [(Double, [(Double, Double)])] -> String
showIt2 = unlines . map (\(t, uvs) -> unwords . map show $ (t : concat [[u, v] | (u, v) <- uvs]))

-- Do it

main :: IO ()
main = do
  writeFile "sine.dat" (showIt1 $ runIt 0.01 1000 $ sine)
  writeFile "sys1.dat" (showIt2 $ runIt 0.01 1000 $ sys1 (0.5, 0.6))