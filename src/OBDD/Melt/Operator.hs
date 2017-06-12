{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE QuasiQuotes          #-}

module OBDD.Melt.Operator where

import qualified Data.Map                as Map
import           Data.String.Interpolate
import           Data.Tree
import Data.List
import           OBDD
import           OBDD.Data
import           OBDD.Graphics.PDF
import           OBDD.Operation
import           Prelude                 hiding (not, (&&), (||))

xor x y = (x || y) && (not (x && y))

data MopKey =
  K {unKey :: String}
  deriving (Ord,Eq,Show)

data MopValue
  = N (Integer,MopKey,MopKey)
  | F [Bool]
  deriving (Show)

type Mop = (Map.Map MopKey MopValue,MopKey)

asTree :: Mop -> Tree String
asTree (m,K r)
  | (Just (F x)) <-
     Map.lookup (K r)
                m = Node r []
asTree (m,K r)
  | (Just (N (n,k1,k2))) <-
     Map.lookup (K r)
                m = Node r [asTree (m,k1),asTree (m,k2)]

instance Show Mop where
  show = drawTree . asTree

evalMop :: Mop -> (Integer -> Bool) -> [Bool]
evalMop (m,r) _
  | (Just (F x)) <- Map.lookup r m = x
evalMop (m,r) e
  | (Just (N (n,k1,k2))) <- Map.lookup r m =
    if (e n)
       then (evalMop (m,k2) e)
       else (evalMop (m,k1) e)
evalMop _ _ = error "Sorry, invalid BDD"

-- convert a bdd by assigning nodes a unique id with prefix t
convert :: OBDD Integer -> MopKey -> Mop
convert n' t =
  case access n' of
    Leaf False ->
      (Map.singleton t
                     (F [False])
      ,t)
    Leaf True ->
      (Map.singleton t
                     (F [True])
      ,t)
    Branch v a b ->
      let kf = K $ unKey t ++ show v ++ "F"
          kt = K $ unKey t ++ show v ++ "T"
          (ff,rf) = convert a kf
          (tt,rt) = convert b kt
          qq =
            Map.singleton t
                          (N (v,kf,kt))
      in (Map.union qq $ Map.union ff tt,t)


getBoolKeys :: Mop -> [MopKey]
getBoolKeys (m,_) = map fst $ filter f (Map.toList m)
  where f (k,F _) = True
        f (k,N _) = False

merge :: Mop -> (MopKey,MopKey) -> Mop
merge (m,r) (k1,k2)
  | (Just (F x),Just (F y)) <- (Map.lookup k1 m,Map.lookup k2 m) =
    if x == y
       then (Map.delete k2 $ Map.map f m,r)
       else (m,r)
  where f (N (n,ka,kb)) = N (n,ka',kb')
          where ka' =
                  if (ka == k2)
                     then k1
                     else ka
                kb' =
                  if (kb == k2)
                     then k1
                     else kb
        f x = x
merge m _ = m

mergeTerminals :: Mop -> Mop
mergeTerminals m =
  let ls = getBoolKeys m
      pairs = [(k1,k2)|k1 <- ls,k2 <- ls,k1 /= k2]
  in foldl merge m pairs

asMop :: OBDD Integer -> Mop
asMop x = mergeTerminals $ convert x (K "R")

label :: MopValue -> String
label (N (v,_,_)) = show v
label (F v) = concatMap s v
  where s True = "1"
        s False = "0"

collectNodeLabels
  :: MopKey -> MopValue -> (MopKey,String,Bool)
collectNodeLabels k x@(N (v,_,_)) = (k,label x,False)
collectNodeLabels k x@(F v) = (k,label x,True)

collectNodesLabel
  :: Mop -> [(MopKey,String,Bool)]
collectNodesLabel (m,r) = Map.foldrWithKey intoList [] m
  where intoList k v l = (collectNodeLabels k v) : l

asNode
  :: (String -> String) -> (MopKey,String,Bool) -> String
asNode convLabel (K key,label,False) =
  [i|#{key}[shape=circle,label=#{convLabel(label)}];
|]
asNode _ (K key,label,True) =
  [i|#{key}[shape=none,label=#{label}];
|]

collectEdges :: Mop -> [(MopKey,MopKey,Bool)]
collectEdges (m,r)
  | (Just (N (n,k1,k2))) <- Map.lookup r m =
    nub $ [(r,k1,False),(r,k2,True)] ++ collectEdges (m,k1) ++ collectEdges (m,k2)
collectEdges _ = []

asEdge :: (MopKey,MopKey,Bool) -> String
asEdge (K from,K to,False) =
  [i|#{from}->#{to}[dir=none,style=dotted];
|]
asEdge (K from,K to,True) =
  [i|#{from}->#{to}[dir=none];
|]

wrapDot s =
  [i|
  digraph BDD {
  node[shape=circle];
  #{s}
  }
|]

combine :: Integer -> Mop -> Mop -> Mop
combine vv (llm, llr) (rrm, rrr) = let
        ada'       = N (vv, llr, rrr)
        kada'      = K $ unKey llr ++ "" ++ unKey rrr
        mp1        = Map.union llm rrm
        mp2        = Map.singleton kada' ada'
        mp12       = Map.union mp1 mp2
        in (mp12, kada')

diamond :: Mop -> Mop -> Mop
diamond aa@(m, or) aa'@(m', or') | (Just (N (v, l, r)), Just (N (v', l', r'))) <- (Map.lookup or m, Map.lookup or' m') =
                                case compare v v' of
                                     EQ -> let
                                        leq = (m,l) `diamond` (m', l')
                                        req = (m,r) `diamond` (m', r')
                                        in combine v leq req
                                     GT -> let
                                        lgt = (m,l) `diamond` aa'
                                        rgt = (m,r) `diamond` aa'
                                        in combine v lgt rgt
                                     LT -> let
                                        llt = aa `diamond` (m', l')
                                        rlt = aa `diamond` (m', r')
                                        in combine v' llt rlt

diamond aa@(m, or) aa'@(m', or') | (Just (N (v, l, r)), Just _) <- (Map.lookup or m, Map.lookup or' m') = let
                                    lll = (m,l) `diamond` aa'
                                    llr = (m,r) `diamond` aa'
                                    in combine v lll llr

diamond aa@(m, or) aa'@(m', or') | (Just _, Just (N (v', l', r'))) <- (Map.lookup or m, Map.lookup or' m') = let
                                        lll = aa `diamond` (m', l')
                                        llr = aa `diamond` (m', r')
                                    in combine v' lll llr

diamond aa@(m, or) aa'@(m', or') | (Just (F l), Just (F l')) <- (Map.lookup or m, Map.lookup or' m') = let
                                    root = F (l ++ l')
                                    key = K $ (unKey or) ++ "" ++ (unKey or')
                                    in (Map.singleton key root, key)

diamond _ _ = error "Unknown error computing meld"

infixl 4 <>
(<>) = diamond



mopAsDot :: Mop -> (String -> String) -> String
mopAsDot (m,r) names = wrapDot (n ++ e)
  where n =
          (concatMap (asNode names)
                     (collectNodesLabel (m,r)))
        e = (concatMap asEdge (collectEdges (m,r)))

plotDotWithLabels
  :: Mop -> (String -> String) -> IO ()
plotDotWithLabels m convLabel = plotDot "final.pdf" (mopAsDot m convLabel)


