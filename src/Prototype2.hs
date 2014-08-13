{-# LANGUAGE NoImplicitPrelude, BangPatterns, ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude, Arrows, IncoherentInstances, Rank2Types #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
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
--type Signal = Applicative

-- Signals are the things, that can be integrated
data Integrable s where
  Integrable1 :: Applicative s =>
                 (forall t a. VectorSpace.C t a => t -> s a -> a -> (s a, a))
                 -> Integrable s
  Integrable2 :: Applicative s =>
                 (forall t a. VectorSpace.C t a => t -> s a -> a -> (s a, a))
                 -> (forall t a. VectorSpace.C t a => t -> s a -> a -> (s a, a))
                 -> Integrable s

class Extractable s where
  extractS :: Applicative s => s a -> a

-- Signal data with Euler method integration, and instances for it.

newtype EU1 v = EU1 v deriving (Show, Read, Eq)

instance Functor EU1 where
  fmap f (EU1 x) = EU1 (f x)

instance Applicative EU1 where
  pure x = EU1 x
  (EU1 f) <*> (EU1 x) = EU1 (f x)

euler1 :: Integrable EU1
euler1 = Integrable1 $ \dt (EU1 x') x0 ->
  let x = x0 + dt *> x'
  in (EU1 x0, x)

instance Extractable EU1 where
  extractS (EU1 x) = x


-- Signal data with implicit Euler method.

newtype IEU1 v = IEU1 [v] deriving (Show, Read, Eq, Functor, Applicative)


implicitEuler1 :: Integrable IEU1
implicitEuler1 = Integrable1 $ \dt (IEU1 xs') x0 ->
  let xs = x0 : fmap (\x' -> x0 + dt *> x') xs' in (IEU1 xs, xs !! 2)

instance Extractable IEU1 where
  extractS (IEU1 xs) = head xs

-- Signal data with implicit Euler method.

newtype IMEU1 v = IMEU1 [v] deriving (Show, Read, Eq, Functor, Applicative)

implicitMidpointEuler1 :: Integrable IMEU1
implicitMidpointEuler1 = Integrable1 $ \dt (IMEU1 xs') x0 ->
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

rungeKutta4 :: Integrable RK4
rungeKutta4 = Integrable1 $
              let two = fromRational 2
                  six = fromRational 6
              in \dt (~(RK4 x1' x2' x3' x4')) x0 ->
              (RK4 x0 (x0 + (dt / two) *> x1') (x0 + (dt / two) *> x2') (x0 + dt *> x3'),
               x0 + (dt / six) *> (x1' + two *> x2' + two *> x3' + x4'))

instance Extractable RK4 where
  extractS = \(RK4 x _ _ _) -> x

-- Symplectic euler

newtype SEU1 v = SEU1 [v] deriving (Show, Read, Eq, Functor, Applicative)

symplecticEuler1 :: Integrable SEU1
symplecticEuler1 =
  Integrable2
  (\dt (SEU1 xs') x0 ->
    let xs = repeat x0 in (SEU1 xs, x0 + dt *> (xs' !! 2)))
  (\dt (SEU1 xs') x0 ->
    let xs = x0 : fmap (\x' -> x0 + dt *> x') xs' in (SEU1 xs, xs !! 2))

instance Extractable SEU1 where
  extractS = \(SEU1 (x : _)) -> x


-- Just a wrapper to make an arrow from "integrate"

integrator :: VectorSpace.C t a => Integrable s -> a -> Continuous t (s a) (s a)
integrator (Integrable1 integrate) x0 = Continuous x0 integrate
integrator (Integrable2 integrate _) x0 = Continuous x0 integrate

integrator2 :: VectorSpace.C t a => Integrable s -> a -> Continuous t (s a) (s a)
integrator2 (Integrable1 integrate) x0 = Continuous x0 integrate
integrator2 (Integrable2 _ integrate) x0 = Continuous x0 integrate


-- Simple sine wave as an example

sine :: Applicative s => Integrable s -> Continuous Double () (s Double)
sine i = proc _ -> do
  rec x <- integrator i 0.0 -< y
      y <- integrator i 1.0 -< liftA negate x
  returnA -< x

sys1 :: Applicative s => Integrable s -> (Double, Double) -> Continuous Double () (s (Double, Double))
sys1 i (u0, v0) = proc _ -> do
  rec u <- integrator i u0 -< liftA2 (*) u (liftA2 (-) v (pure 2))
      v <- integrator i v0 -< liftA2 (*) v (liftA2 (-) (pure 1) u)
  returnA -< liftA2 (,) u v

sys1a :: Applicative s => Integrable s -> (Double, Double) -> Continuous Double () (s (Double, Double))
sys1a i (u0, v0) = proc _ -> do
  rec u <- integrator i u0 -< liftA2 (*) u (liftA2 (-) v (pure 2))
      v <- integrator2 i v0 -< liftA2 (*) v (liftA2 (-) (pure 1) u)
  returnA -< liftA2 (,) u v

-- Run an arrow

runIt :: Double -> Int -> (forall s. Applicative s => Integrable s -> Continuous Double () (s a)) -> [(Double, [a])]
runIt step n block =
  let input = repeat ()
      outputRK = map extractS $ simTrace step input $ block rungeKutta4
      outputEU = map extractS $ simTrace step input $ block euler1
      outputIEU = map extractS $ simTrace step input $ block implicitEuler1
      outputSEU = map extractS $ simTrace step input $ block symplecticEuler1
      outputIMEU = map extractS $ simTrace step input $ block implicitMidpointEuler1
      times = map (* step) [0, 1 ..] :: [Double]
  in take (succ n) $ zip times $ transpose [outputEU, outputRK, outputIEU, outputIMEU]

-- Show the results of the run

showIt1 :: [(Double, [Double])] -> String
showIt1 = unlines . map (\(t, xs) -> unwords . map show $ (t : xs))

showIt2 :: [(Double, [(Double, Double)])] -> String
showIt2 = unlines . map (\(t, uvs) -> unwords . map show $ (t : concat [[u, v] | (u, v) <- uvs]))

-- Do it

main :: IO ()
main = do
  writeFile "sine.dat" (showIt1 $ runIt 0.01 1000 $ sine)
  writeFile "sys1.dat" (showIt2 $ runIt 0.01 1000 $ flip sys1 (0.5, 0.6))
  writeFile "sys1a.dat" (showIt2 $ runIt 0.01 1000 $ flip sys1a (0.5, 0.6))
