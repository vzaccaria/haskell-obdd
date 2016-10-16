
module OBDD.Melt.Test where

import OBDD (unit)
import qualified OBDD.Melt.Operator as M
import OBDD.Melt.Operator (plotDotWithLabels, evalMop, asMop, (<>))
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

type Test = (OBDD Integer, Bool -> Bool -> Bool -> Bool -> Bool)

testtable :: [Test]
testtable = [
  (v1 O.|| v2,                    \b1 b2 b3 b4 -> (b1 || b2)),             
  (v1 O.&& v2,                    \b1 b2 b3 b4 -> (b1 && b2)),             
  ((v1 O.&& v2) `M.xor` v3,       \b1 b2 b3 b4 -> ((b1 && b2) `xor` b3)),    
  (v1 `M.xor` v2 `M.xor` v3,      \b1 b2 b3 b4 -> (b1 `xor` b2 `xor` b3)), 
  (O.not v1,                      \b1 _ _ _ -> not b1),                    
  (M.xor v3 v4,                   \_ _ b3 b4 -> (xor b3 b4)),              
  (M.xor v1 v3,                   \b1 _ b3 b4 -> (xor b1 b3)),             
  (M.xor v3 v1,                   \b1 _ b3 b4 -> (xor b3 b1)),             
  (v1 O.|| v2 O.&& (M.xor v3 v4), \b1 b2 b3 b4 -> (b1 || b2 && (xor b3 b4)))
  ]

testtable1 = [ [p1] | p1 <- testtable ]
testtable2 = [ [p1, p2] | p1 <- testtable, p2 <- testtable ]
testtable3 = [ [p1, p2, p3] | p1 <- testtable, p2 <- testtable, p3 <- testtable ]

-- problematic case
--
pc1 = asMop $ fst $ testtable !! 3
pc2 = asMop $ fst $ testtable !! 4
pc12 = pc1 <> pc2
-- plotDotWithLabels pc12 cl

-- Properties to validate with `quickCheck prop`
--

test3 tsts b1 b2 b3 b4 = res where
  res = evalMop prod var == map (ev . snd) tsts
  prod = foldl1 M.diamond (map (asMop . fst) tsts)
  ev  f = f b1 b2 b3 b4
  var 1 = b1
  var 2 = b2
  var 3 = b3
  var 4 = b4

x `xor` y = (x || y) && (not (x && y))



testGroup1 = testGroup "1-output BDD vs actual TT" $ map (QC.testProperty "*") tests
  where tests = map test3 testtable1 

testGroup2 = testGroup "2-output BDD vs actual 2-TT" $ map (QC.testProperty "*") tests
  where tests = map test3 testtable2

testGroup3 = testGroup "3-output BDD vs actual 3-TT" $ map (QC.testProperty "*") tests
  where tests = map test3 testtable3

tests = testGroup "Tests" [testGroup1, testGroup2, testGroup3]
-- tests = testGroup "Tests" [testGroup1]

test = defaultMain tests
