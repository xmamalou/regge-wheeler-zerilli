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

import Derivatives as D
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

derivative :: (Double -> Double -> Double)
derivative t y = tinyDelta 

-- Runge-Kutta implementation for second order differential equations
{-
 - The following function solves a second order differential equation.
 - It requires to be supplied with the following arguments:
 - 1) The step size
 - 2) The range of the independent variable
 - 3) The initial conditions (yInit, yInit')
 - 4) The function that describes the differential equation (y'' = f(t, y, y'))
 - The solver uses the auxiliary relation y' = g(t, y) to decouple the second order differential equation into two first order differential equations.
 - In each step, it first solves y' = g(t, y) and then y'' = f(t, y, y').
 -}
solveSODiff :: Double -> (Double, Double) -> Double -> Double -> (Double -> Double -> Double -> Double) -> [(Double, Double)]
solveSODiff h (tInit, tFin) yInit yInit' f | tInit >= tFin = []
                                           | tInit < tFin = let (tNext, yNext') = next h tInit y