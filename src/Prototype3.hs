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

data Continuous2 t a b = forall s1 s2. (VectorSpace.C t s1, VectorSpace.C t s2) => Continuous2 !s1 !s2 (a -> (s1, s2) -> (b, s1, s2))

toContinuous2 :: Continuous t a b -> Continuous2 t a b
toContinuous2 (Continuous s f) = Continuous2 s () (\a (s, _) -> let (b, s') = f a s in (b, s', ()))

newtype Solver t = Solver (forall a b s. VectorSpace.C t s => t -> (a -> s -> (b, s)) -> a -> s -> (b, s))

newtype Solver2 t = Solver2 (forall a b s1 s2. (VectorSpace.C t s1, VectorSpace.C t s2) => t -> (a -> (s1, s2) -> (b, s1, s2)) -> a -> (s1, s2) -> (b, s1, s2))

toSolver2 :: Solver t -> Solver2 t
toSolver2 (Solver slv) = Solver2 $ \dt f x (s1, s2) ->
  let three2two = (\(b, s1, s2) -> (b, (s1, s2))) :: (a, b, c) -> (a, (b, c))
      two2three = (\(b, (s1, s2)) -> (b, s1, s2)) :: (a, (b, c)) -> (a, b, c)
  in two2three (slv dt ((three2two .) . f) x (s1, s2))

simStep :: Solver t -> t -> a -> Continuous t a b -> (b, Continuous t a b)
simStep (Solver solver) dt a (Continuous s f) =
  let !(b, s') = solver dt f a s
  in (b, Continuous s' f)

simTrace :: Solver t -> t -> [a] -> (Continuous t a b) -> [b]
simTrace slv dt (x:xs) f = let !(y, f') = simStep slv dt x f
                           in y `seq` (y : simTrace slv dt xs f')
simTrace _ _ [] _ = []

simStep2 :: Solver2 t -> t -> a -> Continuous2 t a b -> (b, Continuous2 t a b)
simStep2 (Solver2 solver) dt a (Continuous2 s1 s2 f) =
  let !(b, s1', s2') = solver dt f a (s1, s2)
  in (b, Continuous2 s1' s2' f)

simTrace2 :: Solver2 t -> t -> [a] -> (Continuous2 t a b) -> [b]
simTrace2 slv dt (x:xs) f = let !(y, f') = simStep2 slv dt x f
                            in y `seq` (y : simTrace2 slv dt xs f')
simTrace2 _ _ [] _ = []


instance Field.C t => Cat.Category (Continuous t) where
  id = Continuous () (\x _ -> (x, ()))
  Continuous sg g . Continuous sf f = Continuous (sg, sf) $ \x (!sg, !sf) ->
    let !(y,sf') = f x sf
        !(z, sg') = g y sg
    in (z, (sg', sf'))

instance Field.C t => Cat.Category (Continuous2 t) where
  id = Continuous2 () () (\x _ -> (x, (), ()))
  Continuous2 sg1 sg2 g . Continuous2 sf1 sf2 f =
    Continuous2 (sg1, sf1) (sg2, sf2) $ \x ((!sg1, !sf1), (!sg2, !sf2)) ->
    let !(y, sf1', sf2') = f x (sf1, sf2)
        !(z, sg1', sg2') = g y (sg1, sg2)
    in (z, (sg1', sf1'), (sg2', sf2'))

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

instance Field.C t => Arrow (Continuous2 t) where
  arr f = Continuous2 () () (\x _ -> (f x, (), ()))

  first (Continuous2 sf1 sf2 f) = Continuous2 sf1 sf2 $ \(x, r) (!s1, !s2) ->
    let !(y, s1', s2') = f x (s1, s2)
    in ((y, r), s1', s2')

  second (Continuous2 sf1 sf2 f) = Continuous2 sf1 sf2 $ \(r, x) (!s1, !s2) ->
    let !(y, s1', s2') = f x (s1, s2)
    in ((r, y), s1', s2')

  (Continuous2 sf1 sf2 f) *** (Continuous2 sg1 sg2 g) =
    Continuous2 (sf1, sg1) (sf2, sg2) $ \(xf, xg) ((!sf1, !sg1), (!sf2, !sg2)) ->
    let !(yf, sf1', sf2') = f xf (sf1, sf2)
        !(yg, sg1', sg2') = g xg (sg1, sg2)
    in ((yf, yg), (sf1', sg1'), (sf2, sg2))

  (Continuous2 sf1 sf2 f) &&& (Continuous2 sg1 sg2 g) =
    Continuous2 (sf1, sg1) (sf2, sg2) $ \x ((!sf1, !sg1), (!sf2, !sg2)) ->
    let !(yf, sf1', sf2') = f x (sf1, sf2)
        !(yg, sg1', sg2') = g x (sg1, sg2)
    in ((yf, yg), (sf1', sg1'), (sf2', sg2'))


instance Field.C t => ArrowLoop (Continuous t) where
  loop (Continuous s f) = Continuous s $ \x !s ->
    let (!(y, r), s') = f (x, r) s
    in (y, s')

instance Field.C t => ArrowLoop (Continuous2 t) where
  loop (Continuous2 s1 s2 f) = Continuous2 s1 s2 $ \x !s ->
    let (!(y, r), s1', s2') = f (x, r) s
    in (y, s1', s2')

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

implicitEuler1 :: Int -> Solver t
implicitEuler1 n = Solver $ \dt f a x0 ->
  let xs = x0 : fmap (\x -> x0 + dt *> (snd $ f a x)) xs
      xf = xs !! n
      (b, _) = f a xf
  in (b, xf)

rungeKutta4 :: Field.C t => Solver t
rungeKutta4 = Solver $ \dt f a s ->
  let (b, k1) = f a s
      (_, k2) = f a (s + (dt / two) *> k1)
      (_, k3) = f a (s + (dt / two) *> k2)
      (_, k4) = f a (s + dt *> k3)
      two = fromRational 2
      six = fromRational 6
  in (b, s + (dt / six) *> (k1 + two *> k2 + two *> k3 + k4))

symplecticEuler1 :: Field.C t => Int -> Solver2 t
symplecticEuler1 n = Solver2 $ \dt f a (s1, s2) ->
  let xs = s2 : fmap (\s2i -> let (_, _, s2') = f a (s1, s2i)
                              in s2 + dt *> s2') xs
      sf2 = xs !! n
      sf1 = let (_, s1', _) = f a (s1, sf2) in s1 + dt *> s1'
      (b, _, _) = f a (sf1, sf2)
  in (b, sf1, sf2)

-- Just a wrapper to make an arrow

integrator :: VectorSpace.C t a => a -> Continuous t a a
integrator x0 = Continuous x0 (\a s -> (s, a))

integrator1 :: VectorSpace.C t a => a -> Continuous2 t a a
integrator1 x0 = Continuous2 x0 () (\a (s, _) -> (s, a, ()))

integrator2 :: VectorSpace.C t a => a -> Continuous2 t a a
integrator2 x0 = Continuous2 () x0 (\a (_, s) -> (s, (), a))

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

sys1a :: (Double, Double) -> Continuous2 Double () (Double, Double)
sys1a (u0, v0) = proc _ -> do
  rec u <- integrator1 u0 -< u * (v - 2)
      v <- integrator2 v0 -< v * (1 - u)
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
      outputs = map (\solver -> simTrace solver step input block) [explicitEuler1, rungeKutta4, implicitEuler1 2]
      times = map (* step) [0, 1 ..] :: [Double]
  in take (succ n) $ zip times $ transpose outputs

runIt2 :: forall a. Double -> Int -> Continuous2 Double () a -> [(Double, [a])]
runIt2 step n block =
  let input = repeat ()
      outputs = map (\solver -> simTrace2 solver step input block) [toSolver2 explicitEuler1, toSolver2 rungeKutta4, toSolver2 (implicitEuler1 2), symplecticEuler1 3]
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
  writeFile "sys1a.dat" (showIt2 $ runIt2 0.01 1000 $ sys1a (0.5, 0.6))
