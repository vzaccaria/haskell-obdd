
module OBDD.Melt.Test where

import OBDD (unit)
import qualified OBDD.Melt.Operator as M
import OBDD.Melt.Operator (plotFinal, plotDotWithLabels, evalMop, asMop)
import OBDD.Data
import qualified OBDD.Operation as O
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Data.Bool ((||),(&&))

-- Variables for the test
v1 = (unit 1 True)
v2 = (unit 2 True)
v3 = (unit 3 True)
v4 = (unit 4 True)

cl :: String -> String 
cl "4" = "v4"
cl "3" = "v3"
cl "2" = "v2"
cl "1" = "v1"

-- Expressions to test

test1mop = asMop $ (v1 O.|| v2)
test1bool b1 b2 _ _ = (b1 || b2)

test2mop = asMop (v1 O.|| v2 O.&& (M.xor v3 v4))
test2bool b1 b2 b3 b4 = (b1 || b2 && (xor b3 b4))

test3mop = test2mop `M.diamond` test1mop
test3bool b1 b2 b3 b4 = [ test2bool b1 b2 b3 b4, test1bool b1 b2 b3 b4]

-- Properties to validate with `quickCheck prop`
--

test1 :: Bool -> Bool -> Bool
test1 b1 b2 = evalMop test1mop var == [test1bool b1 b2 False False] where
  var 1 = b1
  var 2 = b2

x `xor` y = (x || y) && (not (x && y))

test2 :: Bool -> Bool -> Bool -> Bool -> Bool
test2 b1 b2 b3 b4 = evalMop test2mop var == [test2bool b1 b2 b3 b4] where
  var 1 = b1
  var 2 = b2
  var 3 = b3
  var 4 = b4


test3 b1 b2 b3 b4 = evalMop test3mop var == test3bool b1 b2 b3 b4 where
  var 1 = b1
  var 2 = b2
  var 3 = b3
  var 4 = b4


testGroup1 = testGroup "1-output BDD vs actual TT" [
                                                  QC.testProperty "a or b" test1,
                                                  QC.testProperty "a or b and (c xor d)" test2
                                                  ]

testGroup2 = testGroup "2-output BDD vs actual 2-TT" [
                                                  QC.testProperty "[a or b, a or b and (c xor d)]" test3
                                                  ]

tests = testGroup "Tests" [testGroup1, testGroup2]

test = defaultMain tests
