{-# LANGUAGE OverloadedStrings #-}

module Main where
    
import OBDD
import OBDD.Data
import OBDD.Operation
import Control.Lens
import System.Process
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import System.Directory

import Prelude hiding ( (&&), (||), not )

xor x y = (x || y) && (not (x && y))

x1 :: OBDD String
x1 = (unit "S1" True)

x2 :: OBDD String
x2 = (unit "M1" True)

x3 :: OBDD String
x3 = (unit "M2" True)

x4 :: OBDD String
x4 = (unit "M3" True)

-- Knuth's median
test = (not x1) && x2 && x3 || x1 && (x3 || x2)

a :: OBDD Integer 
a = (unit 1 True)

b :: OBDD Integer
b = (unit 2 True)

c :: OBDD Integer
c = (unit 3 True) 

constructed :: OBDD Integer
constructed = a  && b `xor` c

-- Decide how to evaluate leafs and how to combine them at each node
annote :: (Fractional a, Ord t) => OBDD t -> (t -> a) -> a
annote diag p =
  let lfv   True  = 1.0
      lfv   False = 0.0
      brnch node p0 p1 = p node * p1 + (1 - p node) * p0
  in
      fold lfv brnch diag


pj r (-1)  = r
pj _ (-2) = 0.5
pj _ (-3) = 0.5

annote' :: Fractional a => OBDD Integer -> a -> a
annote' exp x = annote exp (pj x)

annoteRange :: (Enum t, Fractional t) => OBDD Integer -> [(t, t)]
annoteRange exp = map (\v -> (v * 0.1, annote' exp (v * 0.1))) [0 .. 10]

exps = [
  a  && b `xor` a && c
  ]


_plot :: LineStyle -> [ (Double, Double)] -> PlotLines Double Double
_plot style dta = plot_lines_style .~ style
  $ plot_lines_values .~ [ dta ]
  $ def

plotProb :: FilePath -> [[(Double, Double)]] -> IO ()
plotProb name dta = do {
  _ <- renderableToFile options name chart;
  return ()
  } where
    h = head dta
    tl = tail dta
    actualSensitiveLine = solidLine lineWidth $ opaque black
    otherLine = solidLine lineWidth $ opaque gray

    layout :: Layout Double Double
    layout = layout_title .~ "P(V=1) varying P(S=1)"
      $ layout_plots .~ toPlot (_plot actualSensitiveLine h) : plotTail
      $ def

    plotTail = map (toPlot . _plot actualSensitiveLine) tl

    chart :: Renderable ()
    chart = toRenderable layout

    options = FileOptions (800, 600) PDF

    lineWidth = 0.25

plotExp :: (Show a) => String -> OBDD a -> IO ()
plotExp name exp = do
    system "rm -rf .bdd";
    createDirectory ".bdd";
    writeFile ".bdd/bdd.dot" $ toDot exp;
    system ("dot -Tpdf .bdd/bdd.dot -o .bdd/prefinal.pdf");
    system ("pdfcrop .bdd/prefinal.pdf " ++ name);
    return ();

plotExp' :: OBDD String -> IO ()
plotExp' = plotExp "final.pdf"

