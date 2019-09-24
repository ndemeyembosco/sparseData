{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}


-- module CombinatorTest where 

import           Control.Monad 
import qualified Data.Vector.Unboxed as U 
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector as VU
import System.Environment (getArgs)
import qualified System.Random.PCG.Fast.Pure as SR 
import Data.Bits 
import System.CPUTime 
import Text.Printf  
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (take) 
import Data.Maybe (fromJust)
import Data.Foldable (foldl', foldr')
import Patterns 
import Data.IORef 
import SparseData 


data ProblemSize = ProblemSize {
   total_keys_size :: Int 
 , max_keys_log    :: Int 
 , num_buckets_log :: Int 
}


getRandomVals ::  Int -> Int -> SR.GenIO -> IO (U.Vector Int)
getRandomVals m n gen = U.replicateM n $ SR.uniformR (0, m) gen

class_a, class_b, class_c, class_d, class_w, class_s :: ProblemSize
class_a = ProblemSize {total_keys_size=23, max_keys_log=19, num_buckets_log=10}
class_b = ProblemSize {total_keys_size=25, max_keys_log=21, num_buckets_log=10}
class_c = ProblemSize {total_keys_size=27, max_keys_log=23, num_buckets_log=10}
class_d = ProblemSize {total_keys_size=31, max_keys_log=27, num_buckets_log=10}
class_w = ProblemSize {total_keys_size=20, max_keys_log=16, num_buckets_log=10}
class_s = ProblemSize {total_keys_size=16, max_keys_log=11, num_buckets_log=10}


getClass :: Char -> Either String ProblemSize
getClass c = case c of 
  'A'  -> Right class_a
  'B'  -> Right class_b
  'C'  -> Right class_c
  'D'  -> Right class_d
  'W'  -> Right class_w 
  'S'  -> Right class_s
  _   -> Left "undefined problem size"

main :: IO ()
main = do
  (size_l : max_it_str : _)  <- getArgs
  let 
    size     = head size_l
    max_iter = read max_it_str :: Int 
  case getClass size of 
    Left err -> print err 
    Right (ProblemSize {
                total_keys_size=arr_length_log
              , max_keys_log=max_elem_log -- might need to change these inputs
              , num_buckets_log=_--radix_log
              }
          ) -> do 
            let 
              digit          = 0  -- digit to sort by, choosing zero in default case.
              arr_length     = 1 `shift` arr_length_log
              max_elem       = 1 `shift` max_elem_log
              -- radix      = 1 `shift` radix_log
            printf  "\n\n NAS Parallel Benchmarks - IS Benchmark\n\n" ;
            printf  " Size:  %d  (class %c)\n" arr_length size  
            printf  " Iterations:   %d\n"  max_iter  
            let seed = 271828183
            my_gen    <- SR.initialize seed 
            to_sort_v <- getRandomVals max_elem arr_length my_gen
            start     <- getCPUTime
            !l1 <- VU.forM (VU.enumFromN 0 max_iter :: VU.Vector Int) $ \iter -> do 
                    let !s = mapParN 1 (* max_elem) to_sort_v
                    printf "%d\n" (iter + 1)
                    return s 
            end <- getCPUTime  
            printf "IS Benchmark Completed\n"
            printf "Class                               %c\n" size 
            printf "Size            =                    %d\n" arr_length
            printf "Iterations      =                    %d\n" max_iter
            let sorted_l = VU.last l1  
            let to_print = U.drop (arr_length - 20) sorted_l
            let diff = (fromIntegral (end - start)) / (10^12)
            print $ "sorted list: " ++ (show  to_print)
            printf "Time in seconds =                    %0.9f sec\n" (diff :: Double)
            printf "Individual time =                  %0.9f sec\n" (diff / fromIntegral max_iter :: Double)
            
