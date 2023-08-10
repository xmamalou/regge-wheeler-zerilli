module Derivatives where

-- derivative operator
delta :: Double -> (Double -> Double) -> (Double -> Double)
delta h f = (\x -> ((f $ (x + h)) - (f $ (x - h)))/(2*h))

deltaMaybe :: Double -> (Double -> Maybe Double) -> (Double -> Maybe Double)
deltaMaybe h f = (\x -> case (f $ (x + h), f $ (x - h)) of
                            (Just yPlus, Just yMinus) -> Just ((yPlus - yMinus)/(2*h))
                            _ -> Nothing)

{-
 - deltaArray can be inaccurate if function f doesn't have many points in a range.
 -}
deltaArray :: [(Double, Double)] -> [(Double, Double)]
deltaArray (x1, y1):(x2, y2):(x3, y3):[] = (x2, (y3 - y1)/(x3 - x1)):[]
deltaArray (x1, y1):(x2, y2):(x3,y3):f = (x2, (y3 - y1)/(x3 - x1)):deltaArray ((x2, y2):(x3, y3):f) 

-- derivative operator with set iterating step
tinyDelta = delta 0.0001
tinyDeltaMaybe = deltaMaybe 0.0001