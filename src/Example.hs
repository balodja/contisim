{-# LANGUAGE NoImplicitPrelude, Arrows, IncoherentInstances, Rank2Types #-}

import NumericPrelude
import Control.Arrow
import Simulation.Continuous

sine :: (Signal s, Integrable s) => Continuous Double () (s Double)
sine = proc _ -> do
  rec x <- integratorS 0.0 -< y
      y <- integratorS 1.0 -< liftS1 negate x
  returnA -< x

{-
vdp :: (Signal s, Integrable s) => Continuous Double (s ()) (s Double)
vdp = proc _ -> do
  rec x <- integratorS 0.0 -< x - liftS1 (/ 3) (x ^ 3) - y
      y <- integratorS 1.0 -< x
  returnA -< x
-}

runIt :: Double -> Int -> (forall s. (Signal s, Integrable s) => Continuous Double () (s Double)) -> [(Double, Double, Double)]
runIt step n block =
  let input = repeat ()
      outputRK = simTrace step input block :: [RK4 Double]
      outputEU = simTrace step input block :: [EU1 Double]
      times = map (* step) [0, 1 ..] :: [Double]
  in take (succ n) $ zip3 times (map extractS outputEU) (map extractS outputRK)

showIt :: [(Double, Double, Double)] -> String
showIt = unlines . map (\(d1, d2, d3) -> unwords . map show $ [d1, d2, d3])

main :: IO ()
main = writeFile "sine.dat" (showIt $ runIt (pi / 10) 10 sine)