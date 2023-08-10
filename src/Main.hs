module Main where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Runge as RK
import Conversions as C

-- time part
{- 
 - v is frequency of the wave
 - t is the time
 - y is the wavefunction
 -}
timePart v t = cos (v*t) + sin (v*t) 

-- coefficient in radial coordinates
q l r = (1 - 1/r)*((l*(l+1))/r^^2 - 3/r^^3)
-- coefficient in Tortoise coordinates
q' l r = toNormalRadial r >>= \r' -> Just $ q l r'

-- differential equation (for the spatial part of the wavefunction)
{- 
 - l in natural numbers
 - v is frequency of the wave
 - r is the radius in tortoise coordinates
 -}
dy' l v r y = (v^^2 + q l r) * y 

-- initial conditions (t = 0)
wavepacket r = exp (-r^^2) -- in tortoise coordinates!

-- file stuff
--fileOptions = FileOptions (800, 600) SVG

main = do
    let swartz = 10
    let tortoiseRad = 0

    print $ RK.solveFODiff 0.001 (0, 5) (wavepacket tortoiseRad) (dy' swartz 1 tortoiseRad)
    {-toFile fileOptions "initial_wavepacket.svg" $ do
    layout_title .= "Runge-Kutta tests"
    setColors [opaque blue]
    plot (line "Runge-Kutta" [RK.solveFODiff 0.001 (0, 5) yInit f])-}