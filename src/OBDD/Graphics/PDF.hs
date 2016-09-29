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

