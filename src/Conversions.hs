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

module Conversions(toNormalTortoise, toNormalRadial, normalize, denormalize) where

import Equations as E

-- logarithm
ln = log

-- converter from Radial to Tortoise - normalized in terms of the Schwarzschild radius
toNormalTortoise :: Double -> Maybe Double
toNormalTortoise r | r >= 1 = Just $ r + ln (r - 1)
                   | r < 1 = Nothing

-- these are used for the converter from Tortoise to Radial
normalRadialEqu :: Double -> (Double -> Maybe Double)
normalRadialEqu r' = (\r -> case (toNormalTortoise r) of 
                                Nothing -> Nothing
                                Just res' -> Just $ r' - res')

fixRangeForProblem range r' = E.rangeFixer 10 (10**(-10)) E.posInf (normalRadialEqu r') range

-- converter from Tortoise to Radial - normalized in terms of the Schwarzschild radius
toNormalRadial :: Double -> Maybe Double
toNormalRadial r' = range >>= \range -> E.maybeSolver' (normalRadialEqu r') range
                where range = if r' < 1 
                                then fixRangeForProblem (1, 1 + 10) r'
                                else fixRangeForProblem (1, r') r'

-- normalizes a radius (either tortoise or radial) in terms of the Schwarzschild radius
normalize :: Double -> Double -> Double
normalize rS r = r/rS

-- denormalize a radius (either tortoise or radial) in terms of the Schwarzschild radius
denormalize :: Double -> Double -> Double
denormalize rS r = rS * r