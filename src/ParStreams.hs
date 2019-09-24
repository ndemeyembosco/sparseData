{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns, DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ParStreams where 

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector as BU 
import qualified Data.Vector.Generic as G 
import qualified Data.Vector.Unboxed as U 
import Control.Monad.Par 
-- import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Combinator (parMap)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive
import Control.Monad 
import Control.DeepSeq 
import GHC.Generics (Generic)


stream :: [a] -> Par [(a, IVar b)]
stream l = do 
    ivars <- replicateM (length l) new 
    return $ zip l ivars 

mapParf :: (a -> b) -> [(a, IVar b)] -> [((a, IVar b), (a -> b))]
mapParf f l = zip l $ replicate (length l) f 

unstream :: NFData b => (a -> b) -> [((a, IVar b))] -> Par [b]
unstream f l = do 
    let ivars = map (\(a, i) -> i) l 
    mapM_ (\(e, i) -> fork $ let ans = f e in put i ans) l 
    mapM get ivars 

mapPar' :: NFData b => (a -> b) -> [a] -> Par [b]
mapPar' f l = do 
    l1 <- stream l 
    unstream f l1  
