{-# LANGUAGE OverloadedStrings #-}

module OBDD.Graphics.PDF where
    
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

plotDot :: String -> String -> IO ()
plotDot name exp = do
    system "rm -rf .bdd";
    createDirectory ".bdd";
    writeFile ".bdd/bdd.dot" $ exp;
    system ("dot -Tpdf .bdd/bdd.dot -o .bdd/prefinal.pdf");
    system ("pdfcrop .bdd/prefinal.pdf " ++ name);
    return ();

plotExp :: (Show a) => String -> OBDD a -> IO ()
plotExp name exp = plotDot name $ toDot exp

plotExp' :: OBDD String -> IO ()
plotExp' = plotExp "final.pdf"

