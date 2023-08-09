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

module Equations(rangeFixer, maybeSolver) where

import Derivatives as D

h = 0.0001

step = 0.0001

posInf = 1/0
negInf = -1/0

avg :: (Double, Double) -> Double
avg (x1, x2) = (x1 + x2) / 2

-- this function fixes the given range so that the Bolzano's theorem holds, knowing that 
-- the function given is decreasing.
rangeFixer :: Double -> Double -> Double -> (Double -> Maybe Double) -> (Double, Double) -> Maybe (Double, Double)
rangeFixer step lo hi f (x1, x2) =
    f x1 >>= \y1 ->
        f x2 >>= \y2 ->
            if y1 * y2 <= 0 
                then Just (x1, x2)
                else if y1 > 0 
                    then if x2 + step >= hi then rangeFixer step lo hi f (x1, hi) else rangeFixer step lo hi f (x1, x2 + step)
                    else if x1 - step <= lo then rangeFixer step lo hi f (lo, x2) else rangeFixer step lo hi f (x1 - step, x2) 

{-
 - maybeSolver always assumes that the range given satisfies Bolzano's theorem.
 - Use rangeFixer to check if a range does indeed satisfy Bolzano's theorem.
 -}
maybeSolver :: Double -> (Double -> Maybe Double) -> (Double, Double) -> Maybe Double
maybeSolver error f (x1, x2) =
    f mean >>= \y ->
    if abs y <= error 
        then Just mean
        else f x2 >>= \y2 -> 
                if y2*y > 0 
                    then maybeSolver error f (x1, mean)
                    else maybeSolver error f (mean, x2)
    where mean = avg (x1, x2)
maybeSolver' = maybeSolver h