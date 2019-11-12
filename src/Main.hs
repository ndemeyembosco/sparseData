{-# LANGUAGE GADTs, FlexibleInstances, BangPatterns, ScopedTypeVariables #-}



module Main where

import SparseData 
import qualified Data.Vector.Unboxed as U  
import Read 
import System.Environment (getArgs)
import qualified System.Random.PCG.Fast.Pure as SR 

getRandomVals ::  Double -> Int -> SR.GenIO -> IO (U.Vector Double)
getRandomVals m n gen = U.replicateM n $ SR.uniformR (0, m) gen


main :: IO ()
main = do 
    args <- getArgs 
    let filename  = head args 
    sp_data <- readIntoSparseData filename 
    let height = width sp_data
    let seed = 271828183
    my_gen    <- SR.initialize seed
    vec     <- getRandomVals 100 height my_gen
    let 
        !q     = (sp_data #+ sp_data) #. vec 
        !alpha = 10 / (q <.> vec)
        !new_z     = q  ^+^ q 
    print q 
    where 
        (<.>) !v1 !v2 = U.sum $! U.zipWith (*) v1 v2  
        (^+^)         = U.zipWith (+)
        (.*) c        = U.map (*c)

