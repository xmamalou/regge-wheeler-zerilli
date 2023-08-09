module Derivatives where

-- derivative operator
delta :: Double -> (Double -> Double) -> (Double -> Double)
delta h f = (\x -> ((f $ (x + h)) - (f $ (x - h)))/(2*h))

deltaMaybe :: Double -> (Double -> Maybe Double) -> (Double -> Maybe Double)
deltaMaybe h f = (\x -> case (f $ (x + h), f $ (x - h)) of
                            (Just yPlus, Just yMinus) -> Just ((yPlus - yMinus)/(2*h))
                            _ -> Nothing)

deltaArray :: Double -> [(Double, Double)] -> [(Double, Double)]
deltaArray h f = map (\(x, y) -> (x, ((y + h) - (y - h))/(2*h))) f

-- derivative operator with set iterating step
tinyDelta = delta 0.0001
tinyDeltaMaybe = deltaMaybe 0.0001
tinyDeltaArray = deltaArray 0.0001