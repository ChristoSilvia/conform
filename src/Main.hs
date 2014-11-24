module Main where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Lens
import Data.Default.Class

import ComplexShapes
import ComplexPlots

main :: IO (PickFn ())
main = renderableToFile def "square.png" (toRenderable (unitCircle 20))
