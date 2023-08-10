module Main where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Runge as RK
import Conversions as C
import Derivatives as D
import Functions as F

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
fileOptions = FileOptions (800, 600) SVG

yInit = packer wavepacket (-100, 100) 0.001
yTimeInit = packer (\x -> 0) (-100, 100) 0.001
yRadInit = packer (tinyDelta wavepacket) (-100, 100) 0.001

stepT = 0.001
rangeT = (0, 5)
stepR = 0.001
rangeR = (-100, 100)

f :: Double -> F.RealFunc -> F.RealFunc -> F.RealFunc -> Equ
f r y yo y' = (\t x xTime xRad -> xTime)

fTime :: Double -> F.RealFunc -> F.RealFunc -> F.RealFunc -> Equ
fTime r y yo y' = (\t x xTime xRad -> F.evaluate (D.deltaArray y') r - x)

fRad :: Double -> F.RealFunc -> F.RealFunc -> F.RealFunc -> Equ
fRad r y yo y' = (\t x xTime xRad -> F.evaluate (D.deltaArray yo) r)

main = do
    let swartz = 10
    let tortoiseRad = 0

    (toFile fileOptions "tests.svg" $ do
        layout_title .= "Runge-Kutta tests"
        setColors [opaque blue]
        plot (line "Runge-Kutta" [RK.getSolutionForT 1 $ RK.solveWave stepT rangeT stepR rangeR (yInit , yTimeInit, yRadInit) (f, fTime, fRad)]))