-- (C) Copyright Chris Banks 2011

-- This file is part of The Continuous Pi-calculus Workbench (CPiWB). 

--     CPiWB is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.

--     CPiWB is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.

--     You should have received a copy of the GNU General Public License
--     along with CPiWB.  If not, see <http://www.gnu.org/licenses/>.

module CpiPlot where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Colour
import Data.Accessor

plot :: [Double] -> [(String,[Double])] -> IO ()
plot ts dims = undefined

{--- test data
testT = [0.0,0.5,1.0,1.5,2.0,2.5,3.0]::[Double]
testD1 = [0,1,2,3,4,5,6,7]::[Double]
testD2 = [0,1,2,4,8,16,32,64]::[Double]

testPlot1 = plot_lines_style ^= solidLine 1 (opaque $ sRGB 0.5 0.5 1)
            $ plot_lines_values ^= [zip testT testD1]
            $ plot_lines_title ^= "test1"
            $ defaultPlotLines

testPlot2 = plot_lines_style ^= solidLine 1 (opaque red)
            $ plot_lines_values ^= [zip testT testD2]
            $ plot_lines_title ^= "test2"
            $ defaultPlotLines

testLayout = layout1_title ^= "Test graph!"
             $ layout1_plots ^= [Left (toPlot testPlot1), 
                                 Left (toPlot testPlot2)]
             $ defaultLayout1

testPlot = renderableToWindow (toRenderable testLayout) 640 480
-}
