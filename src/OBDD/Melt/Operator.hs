{-# LANGUAGE QuasiQuotes #-}
module OBDD.Melt.Operator where 

import OBDD
import OBDD.Data
import OBDD.Operation
import Prelude hiding ( (&&), (||), not )
import Data.String.Interpolate

xor x y = (x || y) && (not (x && y))

data Mop = P (Integer, Mop, Mop, String) | F [ Bool ] deriving (Show)

convert :: OBDD Integer -> String -> Mop
convert n' t = case access n' of
  Leaf False -> F [ False ]
  Leaf True -> F [ True ]
  Branch v a b -> P (v, convert a (t ++ show v ++ "F"), convert b (t ++ show v ++ "T"), t)

a :: OBDD Integer 
a = (unit 1 True)

b :: OBDD Integer
b = (unit 2 True)

c :: OBDD Integer
c = (unit 3 True) 

constructed :: OBDD Integer
constructed = a  && b `xor` c

wrapDot s = [i|
  digraph BDD {
  node[shape=oval];
  #{s}
  }
|]

