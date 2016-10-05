{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module OBDD.Melt.Operator where 

import OBDD
import OBDD.Data
import OBDD.Operation
import OBDD.Graphics.PDF
import Prelude hiding ( (&&), (||), not )
import Data.String.Interpolate

xor x y = (x || y) && (not (x && y))

data Mop = P (Integer, Mop, Mop, String) | F [ Bool ] deriving (Show)

evalMop :: Mop -> (Integer -> Bool) -> [ Bool ]
evalMop (F x) _ = x
evalMop (P (n, f, t, _)) e = if (e n) then (evalMop t e) else (evalMop f e)


asMop :: OBDD Integer -> Mop
asMop x = convert x "R"

convert :: OBDD Integer -> String -> Mop
convert n' t = case access n' of
  Leaf False -> F [ False ]
  Leaf True -> F [ True ]
  Branch v a b -> P (v, convert a (t ++ show v ++ "F"), convert b (t ++ show v ++ "T"), t ++ show v)


label (P (v, _, _, _)) = show v
label (F v) = concatMap s v where
  s True = "1"
  s False = "0"

number (P (_, _, _, n)) = n
number (F v) = concatMap show v where

collectNodes :: Mop -> [ (String, String, Bool) ]
collectNodes x@(P (v, l, r, n) ) = [ (label x, number x, False) ] ++ collectNodes l ++ collectNodes r
collectNodes x@(F v) = [ (label x, number x, True) ]


asNode convLabel (label, number, False) = [i| #{number}[shape=circle,label=#{convLabel(label)}];
|]
asNode _ (label, number, True) = [i| #{number}[shape=none,label=#{label}];
|]

collectEdges :: Mop -> [ (String, String, Bool) ]
collectEdges x@(P (v, l, r, n)) = [ (number x, number l, False), (number x, number r, True) ] ++ collectEdges l ++ collectEdges r
collectEdges _ = []

asEdge :: (String, String, Bool) -> String
asEdge (from, to, False) = [i| #{from}->#{to}[dir=none,style=dotted]; |]
asEdge (from, to, True) = [i| #{from}->#{to}[dir=none]; |]


wrapDot s = [i|
  digraph BDD {
  node[shape=circle];
  #{s}

  }
|]

diamond :: Mop -> Mop -> Mop

diamond a@(P (v, l, r, n)) a'@(P (v', l', r', n'))
  | v == v'                       = P (v, diamond l l', diamond r r',  n ++ "P" ++ n')
  | v > v'                        = P (v, diamond l a', diamond r a',  n ++ "P" ++ n')
  | v < v'                        = P (v', diamond a l', diamond a r', n ++ "P" ++ n')
diamond a@(P (v, l, r, n)) a'     = P (v, diamond l a', diamond r a',  n ++ "P" ++ (number a'))
diamond a a'@(P (v', l', r', n')) = P (v', diamond a l', diamond a r', (number a) ++ "P" ++ (n'))
diamond (F l) (F l') = F ( l ++ l' )

mopAsDot :: Mop -> (String -> String) -> String
mopAsDot m names = wrapDot (n ++ e) where
  n = (concatMap (asNode names) (collectNodes m))
  e = (concatMap asEdge (collectEdges m))


-- For use in documents you have 4 variables with the following labels

s1 = (unit 4 True)
m1 = (unit 3 True)
m2 = (unit 2 True)
m3 = (unit 1 True)

convLabel' :: String -> String 
convLabel' "4" = "S1"
convLabel' "3" = "M1"
convLabel' "2" = "M2"
convLabel' "1" = "M3"

plotDotWithLabels :: Mop -> (String -> String) -> IO ()
plotDotWithLabels m convLabel = plotDot "final.pdf" (mopAsDot m convLabel)

plotFinal :: Mop -> IO ()
plotFinal m = plotDotWithLabels m convLabel'
