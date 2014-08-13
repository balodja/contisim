> {-# LANGUAGE NoImplicitPrelude, Arrows, IncoherentInstances, Rank2Types #-}

> import NumericPrelude
> import Control.Arrow
> import Control.Applicative
> import Simulation.Continuous

Lottka-Volterra model describes an evolution of the population of
the predators $u(t)$ and prey $v(t)$:

\begin{equation}
\left\lbrace
  \begin{matrix}
    \dot{u} & = & u(v-2) \\
    \dot{v} & = & v(1-u)
  \end{matrix}
\right.
\end{equation}

> lotkaVolterra :: (Signal s, Integrable s)
>               => Double -> Double -> Continuous Double () (s (Double, Double))
> lotkaVolterra u0 v0 = proc _ -> do
>   rec u <- integratorS u0 -< liftS2 (*) u (liftS2 (-) v (liftS0 2))
>       v <- integratorS v0 -< liftS2 (*) v (liftS2 (-) (liftS0 1) u)
>   returnA -< liftS2 (,) u v



> runIt :: Double -> Int -> (forall s. (Signal s, Integrable s) => Continuous Double () (s (Double, Double))) -> [(Double, (Double, Double), (Double, Double))]
> runIt step n block =
>  let input = repeat ()
>      outputRK = simTrace step input block :: [RK4 (Double, Double)]
>      outputEU = simTrace step input block :: [EU1 (Double, Double)]
>      times = map (* step) [0, 1 ..] :: [Double]
>  in take (succ n) $ zip3 times (map extractS outputEU) (map extractS outputRK)

> showIt :: [(Double, (Double, Double), (Double, Double))] -> String
> showIt = unlines . map (\(d1, (d2a, d2b), (d3a,d3b)) -> unwords . map show $ [d1, d2a, d2b, d3a, d3b])

> main :: IO ()
> main = writeFile "lv.dat" (showIt $ runIt 0.01 1000 (lotkaVolterra 2 3))
