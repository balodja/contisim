{-# LANGUAGE NoImplicitPrelude, BangPatterns, ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude, Arrows, IncoherentInstances, Rank2Types #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
import NumericPrelude
import Data.List (transpose)
import Control.Arrow
--import Control.Applicative (Applicative, pure, (<*>), liftA, liftA2)
import qualified Control.Category as Cat

import qualified Algebra.Additive as Additive
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Module as Module
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.VectorSpace as VectorSpace

-- Arrow-based streams are the building blocks for simulation

data Continuous t a b = forall s. VectorSpace.C t s => Continuous !s (a -> s -> (b, s))

newtype Solver t = Solver (forall a b s. VectorSpace.C t s => t -> (a -> s -> (b, s)) -> a -> s -> (b, s))

simStep :: Solver t -> t -> a -> Continuous t a b -> (b, Continuous t a b)
simStep (Solver solver) dt a (Continuous s f) =
  let !(b, s') = solver dt f a s
  in (b, Continuous s' f)

simTrace :: Solver t -> t -> [a] -> (Continuous t a b) -> [b]
simTrace slv dt (x:xs) f = let !(y, f') = simStep slv dt x f
                           in y `seq` (y : simTrace slv dt xs f')
simTrace _ _ [] _ = []

instance Field.C t => Cat.Category (Continuous t) where
  id = Continuous () (\x _ -> (x, ()))
  Continuous sg g . Continuous sf f = Continuous (sg, sf) $ \x (!sg, !sf) ->
    let !(y,sf') = f x sf
        !(z, sg') = g y sg
    in (z, (sg', sf'))

instance Field.C t => Arrow (Continuous t) where
  arr f = Continuous () (\x _ -> (f x, ()))

  first (Continuous sf f) = Continuous sf $ \(x, r) !s ->
    let !(y, s') = f x s
    in ((y, r), s')

  second (Continuous sf f) = Continuous sf $ \(r, x) !s ->
    let !(y, s') = f x s
    in ((r, y), s')

  (Continuous sf f) *** (Continuous sg g) = Continuous (sf, sg) $ \(xf, xg) (!sf, !sg) ->
    let !(yf, sf') = f xf sf
        !(yg, sg') = g xg sg
    in ((yf, yg), (sf', sg'))

  (Continuous sf f) &&& (Continuous sg g) = Continuous (sf, sg) $ \x (!sf, !sg) ->
    let !(yf, sf') = f x sf
        !(yg, sg') = g x sg
    in ((yf, yg), (sf', sg'))

instance Field.C t => ArrowLoop (Continuous t) where
  loop (Continuous s f) = Continuous s $ \x !s ->
    let (!(y, r), s') = f (x, r) s
    in (y, s')

instance Additive.C () where
  zero = ()
  () + () = ()
  negate () = ()

instance Ring.C t => Module.C t () where
  x *> () = x `seq` ()

instance Field.C t => VectorSpace.C t ()


explicitEuler1 :: Solver t
explicitEuler1 = Solver $ \dt f a s->
  let (b, s') = f a s in (b, s + dt *> s')

rungeKutta4 :: Field.C t => Solver t
rungeKutta4 = Solver $ \dt f a s ->
  let (b, k1) = f a s
      (_, k2) = f a (s + (dt / two) *> k1)
      (_, k3) = f a (s + (dt / two) *> k2)
      (_, k4) = f a (s + dt *> k3)
      two = fromRational 2
      six = fromRational 6
  in (b, s + (dt / six) *> (k1 + two *> k2 + two *> k3 + k4))

-- Just a wrapper to make an arrow

integrator :: VectorSpace.C t a => a -> Continuous t a a
integrator x0 = Continuous x0 (\a s -> (s, a))

-- Simple sine wave as an example


sine :: Continuous Double () Double
sine = proc _ -> do
  rec x <- integrator 0.0 -< y
      y <- integrator 1.0 -< -x
  returnA -< x

sys1 :: (Double, Double) -> Continuous Double () (Double, Double)
sys1 (u0, v0) = proc _ -> do
  rec u <- integrator u0 -< u * (v - 2)
      v <- integrator v0 -< v * (1 - u)
  returnA -< (u, v)

{-
vdp :: (Signal s, Integrable s) => Continuous Double (s ()) (s Double)
vdp = proc _ -> do
  rec x <- integratorS 0.0 -< x - liftA (/ 3) (x ^ 3) - y
      y <- integratorS 1.0 -< x
  returnA -< x
-}

-- Run an arrow


runIt :: forall a. Double -> Int -> Continuous Double () a -> [(Double, [a])]
runIt step n block =
  let input = repeat ()
      outputs = map (\solver -> simTrace solver step input block) [explicitEuler1, rungeKutta4]
      times = map (* step) [0, 1 ..] :: [Double]
  in take (succ n) $ zip times $ transpose outputs

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
