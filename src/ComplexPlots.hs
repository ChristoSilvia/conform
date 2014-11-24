{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module ComplexPlots where

import Graphics.Rendering.Chart
import Control.Lens
import Data.Default.Class

import ComplexShapes

instance ToRenderable Polygon where
  toRenderable poly = let unitcirc :: PlotLines Double Double
			  unitcirc = plot_lines_values .~ [ (toFigure (unitCircle 40))]
			           $ def
                          polyPlot :: PlotLines Double Double
			  polyPlot = plot_lines_values .~ [( toFigure poly)]
			           $ def

			  layout = layout_plots .~ [(toPlot polyPlot), (toPlot unitcirc)]
			         $ def
		      in toRenderable layout
