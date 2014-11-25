module Main where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Lens
import Data.Default.Class

import Data.List
import Data.Complex
import ComplexShapes
import ComplexPlots
import Mappings

intercircle :: Polygon
intercircle = fmap (zeroOut 0.01) 
            $ circle ((-0.25) :+ 0.0) 0.25 40

lemniscate :: Polygon
lemniscate = nub
           $ adaptiveSqrt (0.1) (0.000001 :+ 0.00000)
	   $ intercircle

main :: IO (PickFn ())
main = do
  print intercircle
  print lemniscate
  print $ length lemniscate
  renderableToFile def "square.png" (toRenderable lemniscate)
