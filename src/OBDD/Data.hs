{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}

-- | implementation of reduced ordered binary decision diagrams.

module OBDD.Data 

(
-- * the data type
 OBDD -- abstract
, size
-- * for external use
, null, satisfiable
, number_of_models
, some_model, all_models
, fold, foldM
-- * for internal use
, Node (..)
, make
, register
, cached, top
, access

)

where

import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM

import OBDD.IntIntMap (IntIntMap)
import qualified OBDD.IntIntMap as IIM

import OBDD.VarIntIntMap (VarIntIntMap)
import qualified OBDD.VarIntIntMap as VIIM

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M

import qualified Data.Array as A

import Data.Set ( Set )
import qualified Data.Set as S

import Control.Monad.State.Strict 
   (State, runState, evalState, get, put)
import qualified System.Random
import Control.Monad.Fix
import Control.Monad ( forM, guard )
import qualified Control.Monad ( foldM )


import Prelude hiding ( null )
import qualified Prelude

-- newtype Index = Index { unIndex :: Int }
--   deriving ( Eq, Ord, Num, Enum, Show )

type Index = Int ; unIndex = id

-- | assumes total ordering on variables
data OBDD v = OBDD
	    { core :: !(IntMap ( Node v Index ))
	    
            -- , icore :: !(Map ( Node v Index ) Index)
            , icore :: !(VarIntIntMap v Index)
	    , next :: !Index
	    , top :: !Index
	    
            , cache ::  !(IntIntMap Index) 
               -- ^ inputs and output for binary op
               -- (unary will be simulated by binary)
	    }

fold :: Ord v 
     => ( Bool -> a )
     -> ( v -> a -> a -> a )
     -> OBDD v -> a
fold leaf branch o =
    let a = A.array (0,top o) $ do
            i <- A.indices a
            return (i, case IM.lookup i $ core o of
                Nothing -> undefined
                Just n  -> case n of
                    Leaf c -> leaf c
                    Branch v l r -> 
                        branch v (a A.! l) (a A.! r) )
    in  a A.! top o

foldM :: (Monad m, Ord v)
     => ( Bool -> m a )
     -> ( v -> a -> a -> m a )
     -> OBDD v -> m a
foldM leaf branch o = do
    f <- leaf False ; t <- leaf True
    let m0 = M.fromList 
           [(icore_false,f), (icore_true,t)]
    m <- Control.Monad.foldM ( \ m (i,n) -> do
            val <- case n of
                Branch v l r -> 
                        branch v (m M.! l) (m M.! r) 
            return $ M.insert i val m
          ) m0 $ IM.toAscList $ core o
    return $ m M.! top o


icore_false = 0 :: Index  
icore_true = 1 :: Index  
              
size = unIndex . next

-- | Number of satisfying assignments with  given set of variables.
-- The set of variables must be given since the current OBDD may not contain
-- all variables that were used to construct it, since some  nodes may have been removed
-- because they had identical children.
number_of_models :: Ord v => Set v -> OBDD v ->  Integer
number_of_models vs o = 
    let fun o vs = do
            m <- get
            case access o of
                   Leaf c -> case c of
                        False -> return 0
                        True -> return $ 2 ^ length vs
                   Branch v l r -> do
                       let ( pre, _ : post ) = span (/= v) vs
                       case M.lookup ( top o ) m of
                          Just x -> return $ ( 2 ^ length pre ) * x
                          Nothing -> do
                             xl <- fun l post
                             xr <- fun r post
                             let xlr = xl + xr
                             m <- get
                             put $! M.insert ( top o ) xlr m
                             return $ ( 2 ^ length pre ) * xlr
    in evalState ( fun o $ reverse $ S.toAscList vs ) M.empty
    

empty :: OBDD v
empty = OBDD 
      { core = IM.empty
      , icore = VIIM.empty
      , next = 2
      , top = 0
      , cache = IIM.empty
      }

data Node v i = Leaf !Bool
	    | Branch !v !i !i
    deriving ( Eq, Ord )

access :: OBDD v -> Node v ( OBDD v )
access s = case top s of
    0 -> Leaf False
    1 -> Leaf True
    t -> case IM.lookup ( top s ) ( core s ) of
        Nothing -> error "OBDD.Data.access"
        Just n  -> case n of
            Leaf p -> error "Leaf in core"
	    Branch v l r -> 
                Branch v ( s { top = l } ) 
                         ( s { top = r } )

-- | does the OBDD have any models?
satisfiable :: OBDD v -> Bool
satisfiable = not . null

-- | does the OBDD not have any models?
null :: OBDD v -> Bool
null s = case access s of
      Leaf False -> True 
      _ -> False

-- | randomly select one model, if possible
some_model :: Ord v 
           => OBDD v 
           -> IO ( Maybe  ( Map v Bool ) )
some_model s = case access s of
    Leaf True -> return $ Just $ M.empty
    Leaf False -> return Nothing
    Branch v l r -> do
        let nonempty_children = do
                 ( p, t ) <- [ (False, l), (True, r) ]        
                 guard $ case access t of
                      Leaf False -> False
                      _ -> True
                 return ( p, t )
        (p, t) <- select_one nonempty_children
        Just m <- some_model t
        return $ Just $ M.insert v p m 

-- | list of all models (WARNING not using 
-- variables that had been deleted)
all_models :: Ord v => OBDD v -> [ Map v Bool ]
all_models s = case access s of
    Leaf True -> return  M.empty
    Leaf False -> [ ]
    Branch v l r -> do
        let nonempty_children = do
                 ( p, t ) <- [ (False, l), (True, r) ]        
                 guard $ case access t of
                      Leaf False -> False
                      _ -> True
                 return ( p, t )
        (p, t) <- nonempty_children
        m <- all_models t
        return $ M.insert v p m 
        
select_one :: [a] -> IO a
select_one xs | not ( Prelude.null xs ) = do
    i <- System.Random.randomRIO ( 0, length xs - 1 )
    return $ xs !! i

make :: State ( OBDD v ) Index
     -> OBDD v
make action = 
    let ( i, s ) = runState action empty 
    in  i `seq` s { top = i }

fresh :: State ( OBDD v ) Index
fresh = do
    s <- get
    let i = next s
    put $! s { next = succ i }
    return i

cached :: Ord v
	=> (Index, Index) 
	-> ( State ( OBDD v ) Index )
	-> State ( OBDD v ) Index
cached (l,r) action = do
    s <- get
    case IIM.lookup (l, r) $ cache s of
        Just i -> return i
	Nothing -> do
	    i <- action
	    s <- get
	    put $! s { cache = IIM.insert (l, r) i 
                              $ cache s }
	    return i

register :: Ord v
	 => Node v Index
       -> State ( OBDD v ) Index
register n = case n of
    Leaf False -> return 0
    Leaf True -> return 1
    Branch v l r -> if l == r then return l else do
      s <- get    
      case VIIM.lookup (v, l, r) ( icore s ) of
        Just i -> return i
	Nothing -> do
	        i <- fresh
	        s <- get
	        put $! s 
		    { core = IM.insert i n $ core s
		    , icore = VIIM.insert (v, l, r) i 
                            $ icore s
		    }
	        return i  
