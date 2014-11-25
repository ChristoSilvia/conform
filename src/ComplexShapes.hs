{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ComplexShapes where

import Data.Complex
import Data.Ord
import Data.List

type Angle = Double
type Length = Double

type NPoints = Int
type Spacing = Double
type Tolerance = Double

class ToFigure a where
  toFigure :: a -> Figure

type Figure = [(Double,Double)]
type Polygon = [Complex Double]

instance ToFigure Polygon where
  toFigure = map (\z -> (realPart z, imagPart z))

smoothen :: Spacing -> Polygon -> Polygon
smoothen interval = nubBy (\w z -> (norm (w-z)) < interval)

zeroOut :: Tolerance -> Complex Double -> Complex Double
zeroOut tol z | (norm z) < tol = 0.0 :+ 0.0
zeroOut tol z                  = z

norm :: Complex Double -> Double
norm z = realPart (abs z)

longest :: Complex Double -> Complex Double -> Ordering
longest a b | (norm a)  > (norm b) = GT
longest a b | (norm a) == (norm b) = EQ
longest a b                        = LT

anglest :: Complex Double -> Complex Double -> Ordering
anglest a b | (phase a)  > (phase b) = GT
anglest a b | (phase a) == (phase b) = EQ
anglest a b                          = LT


circle :: Complex Double -> Length -> NPoints -> Polygon
circle center r n = [ center + (mkPolar r (2*pi*i/n')) | i <- [0.0,1.0..n']]
  where
    n' = fromIntegral n

unitCircle :: NPoints -> Polygon
unitCircle = circle (0.0 :+ 0.0) 1.0


