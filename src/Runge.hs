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
import Functions as F

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

-- Runge-Kutta implementation for a wave equation
newtype 
newtype Equ = Equ (Double -> Double -> Double -> Double -> Double)
newtype BigEqu = BigEqu (F.Func -> F.Func -> F.Func -> Func)

{-
 - This function gives the next value of a wavefunction on a point.
 -}
nextOnPoint :: Double -> Double -> (Double, Double, Double) -> (Equ, Equ, Equ) -> (Double, (Double, Double, Double))
nextOnPoint step tNow (yNow, yTimeNow, yRadNow) (f, fTime, fRad) = (tNext, (yNext, yTimeNext, yRadNext))
                                                                    where yNext = snd $ next step tInit yInit (\t y -> f t y yTimeNow yRadNow)
                                                                          yTimeNext = snd $ next step tInit yInit (\t y -> fTime t yNow y yRadNow) 
                                                                          yRadNext = snd $ next step tInit yRadInit (\t y -> fRad t yNow yTimeNow y)
                                                                          tNext = tInit

{-
 - This function gives the functions f(t + h, y), fTime(t + h, y) and fRad(t + h, y) for all y in a given range.
 - It must be true that the length of the yNow, yTimeNow and yRadNow lists is not only the same,
 - but also equat to (abs (rFin - rInit))/stepR.
 -}
nextOnLine :: Double -> Double -> Double -> (Double, Double) -> (F.Func, F.Func, F.Func) -> (BigEqu, BigEqu, BigEqu) -> [(Double, (Double, Double, Double))]
nextOnLine stepT tNow stepR (rInit, rFin) (yNow, yTimeNow, yRadNow) (f, fTime, fRad) | rInit >= rFin = []
                                                                                     | otherwise = let (tNext, (yNext, yTimeNext, yRadNext)) = nextOnPoint stepT tNow (yImg !! 0, yTimeImg !! 0, yRadImg !! 0) (g, gTime, gRad) in 
                                                                                                        (tNext, (yNext, yTimeNext, yRadNext)):nextOnLine stepT tNow stepR (rInit + stepR, rFin) (tail yNow, tail yTimeNow, tail yRadNow) (f, fTime, fRad)
                                                                                                    where g = f yNow yTimeNow yRadNow
                                                                                                          gTime = fTime yNow yTimeNow yRadNow
                                                                                                          gRad = fRad yNow yTimeNow yRadNow
                                                                                                          yImg = F.image yNow
                                                                                                          yTimeImg = F.image yTimeNow
                                                                                                          yRadImg = F.image yRadNow

{-
 - divider cleanly divides the output of nextOnLine into three Funcs.
 -}                                                                                         
divider :: [(Double, (Double, Double, Double))] -> (F.Func, F.Func, F.Func)
divider [] = (F.Func [], F.Func [], F.Func [])
divider ((t, (y, yTime, yRad)):f) = (F.Func ((t, y):(F.domain yNow)), F.Func ((t, yTime):(F.domain yTimeNow)), F.Func ((t, yRad):(F.domain yRadNow)))
                                    where (yNow, yTimeNow, yRadNow) = divider f
