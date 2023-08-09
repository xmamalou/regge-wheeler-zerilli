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

module Conversions(toTortoise, toRadial) where

import Equations as E

-- logarithm
ln = log

-- converter
toTortoise :: Double ->  Double -> Maybe Double
toTortoise rS r | r >= rS = Just $ r - rS*(1 - ln (r - rS))
                | r < rS = Nothing

radialEqu :: Double -> Double -> (Double -> Maybe Double)
radialEqu rS r' = (\r -> case (toTortoise rS r) of 
                                Nothing -> Nothing
                                Just res' -> Just $ r' - res')

fixRangeForProblem range rS r' = E.rangeFixer 10 (rS + (10**(-10))) posInf (radialEqu rS r') range

toRadial :: Double -> Double -> Maybe Double
toRadial rS r' = range >>= \range -> E.maybeSolver' (radialEqu rS r') range
                where range = if rS < r' 
                                then fixRangeForProblem (rS, rS + 10) rS r'
                                else fixRangeForProblem (rS, r') rS r'