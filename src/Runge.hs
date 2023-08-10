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
type Equ = (Double -> Double -> Double -> Double -> Double)
type BigEqu = (Double -> F.RealFunc -> F.RealFunc -> F.RealFunc -> Equ)

{-
 - This function gives the next value of a wavefunction on a point.
 -}
nextOnPoint :: Double -> Double -> (Double, Double, Double) -> (Equ, Equ, Equ) -> (Double, (Double, Double, Double))
nextOnPoint step tNow (yNow, yTimeNow, yRadNow) (f, fTime, fRad) = (tNext, (yNext, yTimeNext, yRadNext))
                                                                    where yNext = snd $ next step tNow yNow (\t y -> f t y yTimeNow yRadNow)
                                                                          yTimeNext = snd $ next step tNow yNow (\t y -> fTime t yNow y yRadNow) 
                                                                          yRadNext = snd $ next step tNow yRadNow (\t y -> fRad t yNow yTimeNow y)
                                                                          tNext = tNow

{-
 - This function gives the functions f(t + h, y), fTime(t + h, y) and fRad(t + h, y) for all y in a given range.
 - It must be true that the length of the yNow, yTimeNow and yRadNow lists is not only the same,
 - but also (at least) equat to (abs (rFin - rInit))/stepR.
 -}
nextOnLine :: Double -> Double -> Double -> (Double, Double) -> (F.RealFunc, F.RealFunc, F.RealFunc) -> (BigEqu, BigEqu, BigEqu) -> [(Double, (Double, Double, Double))]
nextOnLine stepT tNow stepR (rInit, rFin) (yNow, yTimeNow, yRadNow) (f, fTime, fRad) | rInit >= rFin = []
                                                                                     | otherwise = let (tNext, (yNext, yTimeNext, yRadNext)) = nextOnPoint stepT tNow (yImg !! 0, yTimeImg !! 0, yRadImg !! 0) (g, gTime, gRad) in 
                                                                                                        (tNext, (yNext, yTimeNext, yRadNext)):nextOnLine stepT tNow stepR (rInit + stepR, rFin) (tail yNow, tail yTimeNow, tail yRadNow) (f, fTime, fRad)
                                                                                                    -- the g functions are the f functions with the functions y, yTime and yRad used and evaluated at rInit
                                                                                                    where g = f rInit yNow yTimeNow yRadNow
                                                                                                          gTime = fTime rInit yNow yTimeNow yRadNow
                                                                                                          gRad = fRad rInit yNow yTimeNow yRadNow 
                                                                                                          yImg = F.image yNow
                                                                                                          yTimeImg = F.image yTimeNow
                                                                                                          yRadImg = F.image yRadNow

data Which = First | Second | Third
separator :: Which -> Double -> (Double, Double) -> [(Double, (Double, Double, Double))] -> F.RealFunc
separator _ _ _ [] = []
separator First stepR (rInit, rFin) ((t, (y, yTime, yRad)):f) = (rInit, y):(separator First stepR (rInit + stepR, rFin) f)
separator Second stepR (rInit, rFin) ((t, (y, yTime, yRad)):f) = (rInit, yTime):(separator Second stepR (rInit + stepR, rFin) f)
separator Third stepR (rInit, rFin) ((t, (y, yTime, yRad)):f) = (rInit, yRad):(separator Third stepR (rInit + stepR, rFin) f)
{-
 - divider cleanly divides the output of nextOnLine into three Funcs expressed in terms of RADIUS, with the time as a parameter.
 -}                                                                                         
divider :: Double -> (Double) -> (Double, Double) -> [(Double, (Double, Double, Double))] -> (Double, (F.RealFunc, F.RealFunc, F.RealFunc))
divider tNow stepR range list = (tNow, (separator First stepR range list, separator Second stepR range list, separator Third stepR range list))

nextOnLine' :: Double -> Double -> Double -> (Double, Double) -> (F.RealFunc, F.RealFunc, F.RealFunc) -> (BigEqu, BigEqu, BigEqu) -> (Double, (F.RealFunc, F.RealFunc, F.RealFunc))
nextOnLine' stepT tNow stepR range yNows fs = divider tNow stepR range $ nextOnLine stepT tNow stepR range yNows fs

{-
 - solveWave returns a list of functions of radius, for a range of times.
 - In other words, it solves the wave equation expressed by the functions f, fTime and fRad.
 - 
 - It expects that the equation to be solved can be expressed in the form:
 - dy/dt = f(t, y, dy/dt, d^2y/dr^2)
 - dyTime/dt = fTime(t, y, dy/dt, d^2y/dr^2)
 - dyRad/dt = fRad(t, y, dy/dt, d^2y/dr^2)
 - 
 - Note that the fs written above aren't the same as the fs in the type declaration, but
 - the latter fs do produce the former fs when evaluated for functions y, yTime and yRad, on a given r.
 -}
solveWave :: Double -> (Double, Double) -> Double -> (Double, Double) -> (F.RealFunc, F.RealFunc, F.RealFunc) -> (BigEqu, BigEqu, BigEqu) -> Func Double (F.RealFunc, F.RealFunc, F.RealFunc)
solveWave stepT (tInit, tFin) stepR rRange (yInit, yTimeInit, yRadInit) fs | tInit >= tFin = []
                                                                           | otherwise = let (tNow, (yNow, yTimeNow, yRadNow)) = nextOnLine' stepT tInit stepR rRange (yInit, yTimeInit, yRadInit) fs in
                                                                                            (tNow, (yNow, yTimeNow, yRadNow)):(solveWave stepT (tInit + stepT, tFin) stepR rRange (yNow, yTimeNow, yRadNow) fs)

getSolutionForT :: Double -> Func Double (F.RealFunc, F.RealFunc, F.RealFunc) -> F.RealFunc
getSolutionForT t funcs = let (y1, y2, y3) = (F.evaluate funcs t) in y1
