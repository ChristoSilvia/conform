{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ComplexShapes where

import Data.Complex

type Phase = Double
type Length = Double
type NPoints = Int

class ToFigure a where
  toFigure :: a -> Figure

type Figure = [(Double,Double)]
type Polygon = [Complex Double]

instance ToFigure Polygon where
  toFigure = map (\z -> (realPart z, imagPart z))

circle :: Complex Double -> Length -> NPoints -> Polygon
circle center r n = [ center + (mkPolar r (2*pi*i/n')) | i <- [0.0,1.0..n']]
  where
    n' = fromIntegral n

unitCircle :: NPoints -> Polygon
unitCircle = circle (0.0 :+ 0.0) 1.0


