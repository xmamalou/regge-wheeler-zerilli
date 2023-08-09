module Main where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Runge as RK
import Derivatives as D
import Functions as F
import Equations as E
import Conversions as C

wavepacket r = exp (-r^^2) -- in tortoise coordinates!

-- function stuff
f x = x^^2 + 2*x + 1 
yInit = 1 

-- file stuff
--fileOptions = FileOptions (800, 600) SVG

main = do
    let swartz = 10
    let tortoiseRad = 1000

    print $ C.toRadial swartz tortoiseRad
    {-toFile fileOptions "initial_wavepacket.svg" $ do
    layout_title .= "Runge-Kutta tests"
    setColors [opaque blue]
    plot (line "Runge-Kutta" [RK.solveFODiff 0.001 (0, 5) yInit f])-}