{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module OBDD.Melt.Graph where

import qualified Data.Graph                   as G
import           Data.Graph.Inductive
import           Data.Graph.Inductive.Example
import qualified Data.GraphViz                as DG
import qualified Data.GraphViz.Printing       as DGP
import qualified Data.Map                     as M
import           Data.Text.Lazy                    (unpack)
import           OBDD.Melt.Operator
import           OBDD.Melt.Paper
import OBDD.Graphics.PDF

convertToList :: Mop -> [ (MopValue, MopKey, [MopKey])]
convertToList (m,root) | (Just (F x)) <- M.lookup root m = [(F x, root, [])]
convertToList (m,root) | (Just (N (x,k1,k2))) <- M.lookup root m =
                              [(N (x,k1,k2), root, [k1,k2])] ++ convertToList (m, k1) ++ convertToList (m, k2)

convertToGraph :: Mop -> G.Graph
convertToGraph v = let (g, _, _) = G.graphFromEdges $ convertToList v in g

-- Unlabeled edges and integer labeled nodes
toInductiveGraph :: G.Graph -> Gr () ()
toInductiveGraph g = let
  es = G.edges g
  vs = G.vertices g
  unl = zip vs (repeat ())
  lue = labUEdges es
  in mkGraph unl lue

mopToIG :: Mop -> Gr () ()
mopToIG = toInductiveGraph . convertToGraph

-- instance DG.Labellable () where
--   toLabelValue x = Label $ StrLabel ""

gv2 :: Gr () ()
gv2 = mopToIG v2

pdot :: Mop -> String
pdot x = unpack . DGP.renderDot . DGP.toDot . DG.graphToDot DG.nonClusteredParams $ mopToIG x

pdotFinal :: Mop -> IO ()
pdotFinal x =  plotDot "final.pdf" $ pdot x
