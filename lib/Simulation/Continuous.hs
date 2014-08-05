{-# LANGUAGE NoImplicitPrelude, Arrows #-}

module Simulation.Continuous (
  module Simulation.Continuous.Base
, module Simulation.Continuous.Signal
, ContinuousRK
, ContinuousEU
, integrator
, integratorS
, switch
, watch
, accumulator
, delay
, filterT
, filter1
, filter1Mod
, modFrac
) where

import NumericPrelude
import qualified Algebra.VectorSpace as VectorSpace
import qualified Algebra.RealField as RealField

import Simulation.Continuous.Base
import Simulation.Continuous.Signal
import Control.Arrow

import Data.Maybe (fromMaybe)

type ContinuousRK a b = Continuous Double (RK4 a) (RK4 b)
type ContinuousEU a b = Continuous Double (EU1 a) (EU1 b)

integrator :: VectorSpace.C t a => a -> Continuous t a a
integrator = \y0 -> ContinuousS y0 $ \dt x y1 -> let y2 = y1 + dt *> x in (y1, y2)

integratorS :: (Integrable s, VectorSpace.C t a) => a -> Continuous t (s a) (s a)
integratorS = \x0 -> ContinuousS x0 integrate

switch :: (e -> Continuous t a b) -> e -> Continuous t (a, Maybe e) b
switch = \f e -> ContinuousS (f e) $ \dt (x, es) c -> simStep dt x (maybe c f es)

watch :: (a -> a -> Bool) -> a -> Continuous t (a, b) (Maybe b)
watch = \f x0 -> ContinuousS x0 $ \_ (x2, y) x1 -> (if f x1 x2 then Just y else Nothing, x2)

accumulator :: a -> Continuous t (Maybe a) a
accumulator = \x0 -> ContinuousS x0 $ \_ mx x -> let n = fromMaybe x mx in (n, n)

delay :: a -> Continuous t a a
delay = \x0 -> ContinuousS x0 $ \_ x2 x1 -> (x1, x2)

filter1 :: VectorSpace.C t a => t -> a -> Continuous t a a
filter1 = \t y0 -> let k = recip t
                   in loop $ proc (x, y) -> do
                     y' <- integrator y0 -< k *> (x - y)
                     returnA -< (y', y')

filter1Mod :: (VectorSpace.C t a, RealField.C a) => a -> t -> a -> Continuous t a a
filter1Mod = \m t y0 -> let k = recip t
                            two = fromRational 2
                        in loop $ proc (x, y) -> do
                          y' <- integrator y0 -< k *> (modFrac (x - y + m / two) m - m / two)
                          returnA -< let y'' = modFrac y' m in (y'', y'')

modFrac :: RealField.C t => t -> t -> t
modFrac x x0 = x - floor (x / x0) * x0

filterT :: VectorSpace.C t a => a -> Continuous t (a, t) a
filterT = \y0 -> loop $
  proc ((x, t), y) -> do
    y' <- integrator y0 -< (recip t) *> (x - y)
    returnA -< (y', y')
