{-# LANGUAGE NoImplicitPrelude, BangPatterns, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Simulation.Continuous.Base (
  Continuous(..)
, simStep
, simTrace
) where

import NumericPrelude

import Control.Arrow
import qualified Control.Category as Cat


data Continuous t a b = forall s. ContinuousS !s (t -> a -> s -> (b, s))
                      | ContinuousN (a -> b)

simStep :: t -> a -> (Continuous t a b) -> (b, Continuous t a b)
simStep dt a (ContinuousS s f) = let !(b, s') = f dt a s in (b, ContinuousS s' f)
simStep _ a c@(ContinuousN f) = (f a, c)

simTrace :: t -> [a] -> (Continuous t a b) -> [b]
simTrace dt (x:xs) f = let !(y, f') = simStep dt x f in y `seq` (y : simTrace dt xs f')
simTrace _ [] _ = []

instance Cat.Category (Continuous t) where
  {-# INLINE id #-}
  {-# INLINE (.) #-}
  id = ContinuousN id
  ContinuousS sg g . ContinuousS sf f = ContinuousS (sg, sf) $ \dt x (!sg, !sf) ->
    let !(y, sf') = f dt x sf
        !(z, sg') = g dt y sg
    in (z, (sg', sf'))
  ContinuousS sg g . ContinuousN f = ContinuousS sg $ \dt x !sg ->
    let !(z, sg') = g dt (f x) sg
    in (z, sg')
  ContinuousN g . ContinuousS sf f = ContinuousS sf $ \dt x !sf ->
    let !(y, sf') = f dt x sf
    in (g y, sf')
  ContinuousN g . ContinuousN f = ContinuousN (g . f)


instance Arrow (Continuous t) where
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}
  {-# INLINE arr #-}
  arr f = ContinuousN f
  
  first (ContinuousS sf f) = ContinuousS sf $ \dt (x, r) !s ->
    let !(y, s') = f dt x s
    in ((y, r), s')
  first (ContinuousN f) = ContinuousN (first f)

  second (ContinuousS sf f) = ContinuousS sf $ \dt (r, x) !s ->
    let !(y, s') = f dt x s
    in ((r, y), s')
  second (ContinuousN f) = ContinuousN (second f)

  (ContinuousS sf f) *** (ContinuousS sg g) = ContinuousS (sf, sg) $ \dt (xf, xg) (!sf, !sg) ->
    let !(yf, sf') = f dt xf sf
        !(yg, sg') = g dt xg sg
    in ((yf, yg), (sf', sg'))
  (ContinuousS sf f) *** (ContinuousN g) = ContinuousS sf $ \dt (xf, xg) !sf ->
    let !(yf, sf') = f dt xf sf
    in ((yf, g xg), sf')
  (ContinuousN f) *** (ContinuousS sg g) = ContinuousS sg $ \dt (xf, xg) !sg ->
    let !(yg, sg') = g dt xg sg
    in ((f xf, yg), sg')
  (ContinuousN f) *** (ContinuousN g) = ContinuousN (f *** g)

  (ContinuousS sf f) &&& (ContinuousS sg g) = ContinuousS (sf, sg) $ \dt x (!sf, !sg) ->
    let !(yf, sf') = f dt x sf
        !(yg, sg') = g dt x sg
    in ((yf, yg), (sf', sg'))
  (ContinuousS sf f) &&& (ContinuousN g) = ContinuousS sf $ \dt x !sf ->
    let !(yf, sf') = f dt x sf
    in ((yf, g x), sf')
  (ContinuousN f) &&& (ContinuousS sg g) = ContinuousS sg $ \dt x !sg ->
    let !(yg, sg') = g dt x sg
    in ((f x, yg), sg')
  (ContinuousN f) &&& (ContinuousN g) = ContinuousN (f &&& g)

instance ArrowLoop (Continuous t) where
  {-# INLINE loop #-}
  loop (ContinuousS s f) = ContinuousS s $ \dt x !s ->
    let (!(y, r), s') = f dt (x, r) s
    in (y, s')
  loop (ContinuousN f) = ContinuousN (\x -> let (y, r) = f (x, r) in y)

{-
instance ArrowInit Continuous where
  {-# INLINE init #-}
  {-# INLINE loopD #-}
  init i = ContinuousS i $ \dt x s -> (s, x)
  loopD i f = ContinuousS i $ \dt b s ->
    let !cs = f (b, s) in cs
-}