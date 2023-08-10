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

module Functions where

type Func x y = [(x, y)]
type RealFunc = Func Double Double
{- packer takes a function f of type x -> y, where x is an ordered field or ring and a range in the form of a tuple,
 - and a step and spits out an array of tuples where the first member is the x and the second the f(x) 
 -} 
packer :: (Num x, Ord x, Eq x) => (x -> y) -> (x, x) -> x -> Func x y
packer f (xInit, xFin) step | xInit >= xFin = []
                            | xInit < xFin = (xInit, f xInit):(packer f (xInit + step, xFin) step)

{-
 - Get the image of a function
 -}
image :: Func x y -> [y]
image f = map snd f

{-
 - Get the domain of a function
 -}
domain :: Func x y -> [x]
domain f = map fst f

{-
 - Evaluate a function at a point
 -}
evaluate :: (Eq x) => Func x y -> x -> y
evaluate f x = head $ image $ filter (\(x', y) -> x' == x) f