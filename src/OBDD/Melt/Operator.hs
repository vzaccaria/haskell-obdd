{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
module OBDD.Melt.Operator where 

import OBDD
import OBDD.Data
import OBDD.Operation
import OBDD.Graphics.PDF
import qualified Data.Map as Map
import Prelude hiding ( (&&), (||), not )
import Data.String.Interpolate

xor x y = (x || y) && (not (x && y))

data MopKey  = K { unKey::String } deriving (Ord,Eq,Show)
data MopValue = N (Integer, MopKey, MopKey) | F [ Bool ] deriving (Show)
type Mop = Map.Map MopKey MopValue 


evalMop :: Mop -> MopKey -> (Integer -> Bool) -> [Bool]
evalMop m r _ | (Just (F x))           <- Map.lookup r m = x
evalMop m r e | (Just (N (n, k1, k2))) <- Map.lookup r m = if (e n) then (evalMop m k2 e) else (evalMop m k1 e)
evalMop _ _ _ = error "Sorry, invalid BDD"

-- convert a bdd by assigning nodes a unique id with prefix t
convert :: OBDD Integer -> MopKey -> Mop
convert n' t = case access n' of
  Leaf False -> Map.singleton t (F [ False ]) 
  Leaf True ->  Map.singleton t (F [ True ])
  Branch v a b -> let kf = K $ unKey t ++ show v ++ "F"
                      kt = K $ unKey t ++ show v ++ "T"
                      ff = convert a kf
                      tt = convert b kt
                      qq = Map.singleton t (N (v, kf, kt)) in
                    Map.union qq $ Map.union ff tt 

getBoolKeys :: Mop -> [ MopKey ]
getBoolKeys m = map (fst) $ filter f (Map.toList m) where 
  f (k, (F _)) = True
  f (k, (N _)) = False

merge :: Mop -> (MopKey, MopKey) -> Mop
merge m (k1, k2) | (Just (F x), Just (F y)) <- (Map.lookup k1 m, Map.lookup k2 m) =
                  if x == y
                    then Map.delete k2 $ Map.map f m
                  else
                    m
  where
                    f (N (n, ka, kb)) = N (n, ka', kb') where
                      ka' = if (ka == k2) then k1 else ka
                      kb' = if (kb == k2) then k1 else kb
                    f x = x
merge m _ = m

mergeTerminals :: Mop -> Mop
mergeTerminals m = let ls = getBoolKeys m
                       pairs = [ (k1, k2) | k1 <- ls, k2 <- ls, k1 /= k2 ]
                   in foldl merge m pairs


asMop :: OBDD Integer -> Mop
asMop x = convert x (K "R")


label :: MopValue -> String
label (N (v, _, _)) = show v
label (F v) = concatMap s v where
  s True = "1"
  s False = "0"

collectNodeLabels :: MopKey -> MopValue -> (MopKey, String, Bool)
collectNodeLabels k x@(N (v, _, _)) = (k, label x, False)
collectNodeLabels k x@(F v)         = (k, label x, True)

collectNodesLabel :: Mop -> [(MopKey, String, Bool)]
collectNodesLabel = Map.foldrWithKey intoList [] where
  intoList k v l = (collectNodeLabels k v):l


asNode :: (String -> String) -> (MopKey, String, Bool) -> String
asNode convLabel (K key, label, False) = [i|#{key}[shape=circle,label=#{convLabel(label)}];
|]
asNode _ (K key, label, True) = [i|#{key}[shape=none,label=#{label}];
|]

collectEdges :: Mop -> MopKey -> [ (MopKey, MopKey, Bool) ]
collectEdges m r | (Just (N (n, k1, k2))) <- Map.lookup r m =
                     [ (r, k1, False), (r, k2, True) ] ++ collectEdges m k1 ++ collectEdges m k2
collectEdges _ _ = []

asEdge :: (MopKey, MopKey, Bool) -> String
asEdge (K from, K to, False) = [i|#{from}->#{to}[dir=none,style=dotted];
|]
asEdge (K from, K to, True) =  [i|#{from}->#{to}[dir=none];
|]


wrapDot s = [i|
  digraph BDD {
  node[shape=circle];
  #{s}
  }
|]

-- diamond :: Mop -> Mop -> Mop

-- diamond a@(P (v, l, r, n)) a'@(P (v', l', r', n'))
--   | v == v'                       = P (v, diamond l l', diamond r r',  n ++ "P" ++ n')
--   | v > v'                        = P (v, diamond l a', diamond r a',  n ++ "P" ++ n')
--   | v < v'                        = P (v', diamond a l', diamond a r', n ++ "P" ++ n')
-- diamond a@(P (v, l, r, n)) a'     = P (v, diamond l a', diamond r a',  n ++ "P" ++ (number a'))
-- diamond a a'@(P (v', l', r', n')) = P (v', diamond a l', diamond a r', (number a) ++ "P" ++ (n'))
-- diamond (F l) (F l') = F ( l ++ l' )

mopAsDot :: Mop -> (String -> String) -> String
mopAsDot m names = wrapDot (n ++ e) where
  n = (concatMap (asNode names) (collectNodesLabel m))
  e = (concatMap asEdge (collectEdges m (K "R")))



m = asMop (s1 `xor` m1)
x = mopAsDot m convLabel'

plotDotWithLabels :: Mop -> (String -> String) -> IO ()
plotDotWithLabels m convLabel = plotDot "final.pdf" (mopAsDot m convLabel)

plotFinal :: Mop -> IO ()
plotFinal m = plotDotWithLabels m convLabel'

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
