-- (C) Copyright Chris Banks 2011-2012

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
import qualified Control.Exception as X
import qualified Numeric.LinearAlgebra as LA

import CpiLib

plotTimeSeries ts soln ss
    = plot 
      (LA.toList ts) 
      (zip (map pretty ss) (map LA.toList (LA.toColumns soln)))

plot :: [Double] -> [(String,[Double])] -> IO ()
-- Plots the time series in a GTK window
plot ts dims = renderableToWindow (toRenderable (layout ts dims)) 640 480

-- gets a plot layout with plots for each dimension
layout ts dims = layout1_plots ^= plots ts (colours (length dims)) dims $
                 defaultLayout1

plots :: [Double] -> [AlphaColour Double] -> [(String,[Double])] -> 
         [Either (Plot Double Double) b]
-- gets the plots for each dimension
plots _ _ [] = []
plots ts (colour:cs) ((lbl,pts):dims) 
    = (Left $ toPlot $
       plot_lines_style ^= solidLine 1 colour $
       plot_lines_values ^= [zip ts pts] $
       plot_lines_title ^= lbl $
       defaultPlotLines
      ) : plots ts cs dims
plots _ [] _ = X.throw $ CpiException 
               "CpiPlot.plots: Run out of colours!"

colours :: Int -> [AlphaColour Double]
-- gives n visually distinct colours
-- algorithm taken from the MATLAB 'varycolor' function
-- by Daniel Helmick: http://j.mp/xowLV2
colours n
    | n<=0 = []
    | n==1 = [clr 0 1 0]
    | n==2 = [clr 0 1 0,clr 0 1 1]
    | n==3 = [clr 0 1 0,clr 0 1 1,clr 0 0 1]
    | n==4 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1]
    | n==5 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1,clr 1 0 0]
    | n==6 = [clr 0 1 0,clr 0 1 1,clr 0 0 1,clr 1 0 1,clr 1 0 0,clr 0 0 0]
    | otherwise = sec 1 ++ sec 2 ++ sec 3 ++ sec 4 ++ sec 5
    where
      s = fromIntegral(n `div` 5)
      e = fromIntegral(n `mod` 5)
      f x y
          | x<=y = 1.0
          | otherwise = 0.0
      g x = [1..(s+(f x e))]
      sec x
          | x==1 = [clr 0 1 ((m-1)/(s+(f x e)-1)) | m<-g x]
          | x==2 = [clr 0 ((s+(f x e)-m)/(s+(f x e))) 1 | m<-[1..(s+(f x e))]]
          | x==3 = [clr (m/(s+(f x e))) 0 1 | m<-[1..(s+(f x e))]]
          | x==4 = [clr 1 0 ((s+(f x e)-m)/(s+(f x e))) | m<-[1..(s+(f x e))]]
          | x==5 = [clr ((s+(f x e)-m)/(s+(f x e))) 0 0 | m<-[1..(s+(f x e))]]
          | otherwise = undefined

clr :: Double -> Double -> Double -> AlphaColour Double
clr r g b = opaque(sRGB r g b)


{-- test data
testT = [0.0,0.1..2500.0]::[Double]
testD1 = [0,0.1..2500]::[Double]
testD2 = [x*x|x<-[0,0.1..2500]]::[Double]

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
