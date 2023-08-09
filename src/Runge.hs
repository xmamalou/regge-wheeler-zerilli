{-
Copyright 2023 Christopher-Marios Mamaloukas

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

module Runge where

-- Runge-Kutta implementation
k :: Int -> Double -> Double -> Double -> (Double -> Double -> Double) -> Double
k 1 h t y f = f t y
k 2 h t y f = f (t + h/2) (y + (h/2)*(k 1 h t y f))
k 3 h t y f = f (t + h/2) (y + (h/2)*(k 2 h t y f))
k 4 h t y f = f (t + h) (y + h*(k 3 h t y f))

k1 = k 1
k2 = k 2
k3 = k 3
k4 = k 4

next :: Double -> Double -> Double -> (Double -> Double -> Double) -> (Double, Double)
next h tNow yNow f = (tNow + h, yNow + (h/6)*((k1 h tNow yNow f) + 2*(k2 h tNow yNow f) + 2*(k3 h tNow yNow f) + (k4 h tNow yNow f)))

solveFODiff :: Double -> (Double, Double) -> Double -> (Double -> Double -> Double) -> [(Double, Double)]
solveFODiff h (tInit, tFin) yInit f | tInit >= tFin = []
                                | tInit < tFin = let (tNext, yNext) = next h tInit yInit f in 
                                                        (tNext, yNext):(solveFODiff h (tNext, tFin) yNext f) 
