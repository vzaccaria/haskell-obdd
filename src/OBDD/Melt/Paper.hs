
module OBDD.Melt.Paper where

import qualified Prelude as P
import Prelude (Bool(..),String(..),IO(..),($))
import OBDD (unit,(||),(&&),not)
import qualified OBDD.Melt.Operator as M
import OBDD.Melt.Operator (plotDotWithLabels, evalMop, asMop, (<>))
import OBDD.Data
import qualified OBDD.Operation as O
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

xor x y = (x || y) && (not (x && y))

-- -- For use in documents you have 4 variables with the following labels
s1 = (unit 4 True)
m1 = (unit 3 True)
m2 = (unit 2 True)
m3 = (unit 1 True)

convLabel' :: String -> String
convLabel' "4" = "S1"
convLabel' "3" = "M1"
convLabel' "2" = "M2"
convLabel' "1" = "M3"

v1 = asMop $ (s1 && m1) `xor` m2
v2 = asMop $ (s1 `xor` m2 `xor` m3)
v3 = asMop $ (s1 `xor` m3)

plotFinal :: M.Mop -> IO ()
plotFinal m = plotDotWithLabels m convLabel'

p = plotFinal
plot = plotFinal

-- Problematic case presenting duplications:
---
-- p v2

