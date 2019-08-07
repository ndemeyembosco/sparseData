{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.Vector.Unboxed as U 
--(freeze, take)  
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector as VU
import System.Environment (getArgs)
import qualified System.Random.PCG.Fast.Pure as SR 
import Data.Bits 
-- import System.CPUTime 
import System.Clock 
import Text.Printf  
import Control.Parallel.Strategies 
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (take) 


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


countFreqs :: V.IOVector Int -> U.Vector Int -> IO (V.IOVector Int)
{-# INLINE countFreqs #-}
countFreqs !temp !v = do 
                        U.forM_ v $ \x -> do
                                  let digit_of_Ai = x -- (x `div` radix^digit) `mod` radix
                                  tx <- V.read temp digit_of_Ai
                                  V.write temp digit_of_Ai (tx + 1)
                        return temp 
                           


accumFreqs :: V.IOVector Int -> Int -> IO (V.IOVector Int)
{-# INLINE accumFreqs #-}
accumFreqs !temp !max_elem = do 
                               U.forM_ (U.enumFromN 1 max_elem) $ \count -> do
                                                    val   <- V.read temp count
                                                    val_m <- V.read temp (count - 1)
                                                    V.write temp count (val + val_m)
                               return temp  
                                  

writeSortedL :: V.IOVector Int -> V.IOVector Int -> U.Vector Int -> IO (V.IOVector Int)
{-# INLINE writeSortedL #-}
writeSortedL !temp !sorted_l !to_sort = do 
                                          let 
                                            l = U.length to_sort
                                            v = U.enumFromN 1 l
                                          U.forM_ v $ \i -> do
                                                            let x = to_sort U.! (l - i)
                                                            let d = x -- (x `div` radix^digit) `mod` radix
                                                            tx <- V.read temp d
                                                            V.write temp d (tx - 1)
                                                            cx <- V.read temp d
                                                            V.write sorted_l cx x
                                          return sorted_l
                                                      


-- mutate_strat :: V.Unbox a => Strategy [[a]] -> Strategy (IO [V.IOVector a])
-- mutate_strat strat = return $ do 
--                          l <- runEvalIO strat 
--                          l1 <- join $ (U.thaw . U.fromList) <$> l 
--                          return l1 
                      


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
            start     <- getTime ProcessCPUTime
            !(l1 :: [V.IOVector Int]) <- forM [0..max_iter] $! \iter -> do 
                                                    !to <- runEvalIO $ do 
                                                      !temp_l1    <- rpar $ V.replicate (max_elem + 1) (0 :: Int)  
                                                      !sorted_l1  <- rseq $ V.replicate arr_length (0 :: Int)  -- list of sorted elements to return
                                                      let 
                                                        temp_l = unsafePerformIO temp_l1
                                                        sorted_l = unsafePerformIO sorted_l1
                                                      !t <- rseq $ countFreqs temp_l to_sort_v 
                                                      !f <- rseq $ accumFreqs (unsafePerformIO t) max_elem   
                                                      !s <- rpar $ writeSortedL (unsafePerformIO f) sorted_l to_sort_v
                                                      return s 
                                                    print iter 
                                                    return (unsafePerformIO to) 
                    
            
            end <- getTime ProcessCPUTime
            printf "IS Benchmark Completed\n"
            printf "Class           =                    %c\n" size 
            printf "Size            =                    %d\n" arr_length
            printf "Iterations      =                    %d\n" max_iter
            -- let sorted_l = last l1  
            -- to_print <- U.unsafeFreeze $ V.drop (arr_length - 20) sorted_l
            let diff = (fromIntegral ((nsec end) - nsec start)) / (10^9)
            -- print $ "sorted list: " ++ (show  to_print)
            printf "Time in seconds =                    %0.9f sec\n" (diff :: Double)
            printf "Individual time =                    %0.9f sec\n" (diff / fromIntegral max_iter :: Double)
            
