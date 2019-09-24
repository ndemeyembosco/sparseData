{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

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

merge :: U.Vector Int -> U.Vector Int  -> U.Vector Int 
{-# INLINE merge #-}
merge !v1 !v2  = unsafePerformIO $ do  
      to_return <- V.new fin_len
      mv1       <- U.unsafeThaw v1 
      mv2       <- U.unsafeThaw v2 
      go 0 0 0 mv1 mv2 to_return
  where
    len_v1 = U.length v1 
    len_v2 = U.length v2 
    fin_len = len_v1 + len_v2 
    {-# INLINE go #-}
    go i j k vec1 vec2 to_return 
                    | (i < len_v1 && j < len_v2) = do 
                                    e1 <- V.read vec1 i 
                                    e2 <- V.read vec2 j 
                                    if e1 <= e2 
                                      then do 
                                        V.write to_return k e1 
                                        go (i + 1) j (k + 1) vec1 vec2 to_return
                                      else do 
                                        V.write to_return k e2 
                                        go i (j + 1) (k + 1) vec1 vec2 to_return
                    | i < len_v1 = do 
                      e <- V.read vec1 i 
                      V.write to_return k e 
                      go (i + 1) j (k + 1) vec1 vec2 to_return
                    | j < len_v2 = do 
                      e <- V.read vec2 j 
                      V.write to_return k e 
                      go i (j + 1) (k + 1) vec1 vec2 to_return
                    | otherwise = do 
                      v_to_return <- U.unsafeFreeze to_return
                      return v_to_return

local_sort :: Int -> Int ->  U.Vector Int -> U.Vector Int 
{-# INLINE local_sort #-}
local_sort !arr_length !max_elem !to_sort_v = unsafePerformIO $ do 
      !temp_l    <- V.replicate (max_elem + 1) (0 :: Int)
      !sorted_l  <- V.replicate arr_length (0 :: Int)  -- list of sorted elements to return
      !freqs     <- countFreqs temp_l to_sort_v 
      !acc_fs    <- accumFreqs freqs max_elem
      !ans_fs    <- writeSortedL acc_fs sorted_l to_sort_v 
      !vec <- U.unsafeFreeze ans_fs
      return vec 

global_sort :: Int -> Int -> Int -> U.Vector Int -> (U.Vector Int) 
{-# INLINE global_sort #-}
global_sort !par_n !arr_length !max_elem !to_sort_v = unsafePerformIO $ do 
                  let !ans = parDivConqGenV to_sort_v par_n (\v -> local_sort (U.length v) (U.maximum v) v)  merge 
                  return ans 
                                
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
                    let !s = global_sort 8 arr_length max_elem to_sort_v
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
            
