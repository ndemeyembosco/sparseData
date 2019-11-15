{-# Language ScopedTypeVariables #-}
{-# Language BangPatterns #-}
import Prelude 
import qualified Data.Vector as BV  
import qualified Data.Vector.Unboxed as U 
import  qualified Data.Vector.Unboxed.Mutable as V  
import qualified Control.Monad as M hiding (sequence)
import Data.IORef 
import System.Environment (getArgs)
import Control.Monad.Primitive 
import qualified Control.Monad as A 
import System.CPUTime 
import Text.Printf 
import Control.Parallel.Strategies 
import qualified System.Random.PCG.Fast.Pure as SR 


class_a :: Int 
class_a = 2^28

class_b :: Int 
class_b = 2^30 

getRandomVals ::  Int -> SR.GenIO -> IO (U.Vector Double)
getRandomVals n gen = U.replicateM n $ SR.uniformR (0.0, 1.0) gen 


getClass :: Char -> Either String Int 
getClass c = case c of 
  'A'  -> Right class_a
  'B'  -> Right class_b
  w    -> Left $ "undefined problem size " ++ (show w)


gauss_uni_distr :: Int -> U.Vector Double -> IO (BV.Vector Int, Double, Double, Int)
{-# INLINE gauss_uni_distr #-}
gauss_uni_distr !c !rand_vals = do 
                    to_return <- V.new c 

                    -- initialize references for output 
                    (map_refs :: BV.Vector (IORef Int)) <- BV.replicateM 10 $ newIORef 0
                    x_sum_r  <- newIORef 0 
                    y_sum_r  <- newIORef 0
                    gauss_c  <- newIORef 0

                    -- perform main computation as specified on NAS website 
                    let to_loop_on = [1..c]
                    A.forM_ to_loop_on $ \j -> do 
                            let x_j = (2 * rand_vals U.! (2*j - 1)) - 1 
                            let y_j = (2 * rand_vals U.! (2*j)) - 1 
                            let t_j = (x_j^2) + (y_j^2)  
                            let new_coeff c1 = (*) (c1) $ (sqrt ((-2.0 * log (t_j)) / (t_j)))
                            if t_j <= 1 then 
                              do 
                                let x_k = new_coeff x_j 
                                let y_k = new_coeff y_j
                                V.write to_return (j - 1) (x_k, y_k)
                                modifyIORef' x_sum_r (+ x_k)
                                modifyIORef' y_sum_r (+ y_k)
                                modifyIORef' gauss_c (+ 1)
                                let m = max (abs x_k) (abs y_k)

                                -- increment each of the references in the list 
                                -- if the value m falls in the square annulus l <= max (|x_k|, |y_k|) <= l
                                BV.forM_ (BV.enumFromN 0 9) $ \l -> do 
                                  if (fromIntegral l <= m && m < (fromIntegral l + 1.0)) 
                                  then 
                                    do 
                                      let (len_ref :: IORef Int) = map_refs BV.! l 
                                      modifyIORef' len_ref (+1)
                                      return ()
                                  else return ()
                                return ()
                            else 
                              return ()

                    -- retrieve values from references and return them 
                    to_r_ret <- BV.forM map_refs $ \r -> do 
                                          val <- readIORef r 
                                          return val 
                    x_sum <- readIORef x_sum_r
                    y_sum <- readIORef y_sum_r
                    count <- readIORef gauss_c
                    return (to_r_ret, x_sum, y_sum, count)

main :: IO ()
main = do 
  (size_l : _)  <- getArgs
  let 
    size     = head size_l
  case getClass size of 
        Left err  -> print err 
        Right c   -> do 
              let rand_nums = (2*c) + 1

              start <- getCPUTime 
              print "start timing ..."
              let seed = 271828183
              my_gen    <- SR.initialize seed 
              !rand_vals <- getRandomVals rand_nums my_gen 
              end2 <- getCPUTime
              p <- gauss_uni_distr c rand_vals
              (!may_xys, !sumxs, !sumys, !pairs) <- runEvalIO $ parTuple4 (parTraversable rpar) rseq rpar rpar $ p  
              end <- getCPUTime

              print "end timing ..."
              printf "\n\n\n"
              printf "NAS Parallel Benchmarks - EP Benchmark \n"
              printf "Number of random numbers generated\t%d\n" rand_nums
              printf "EP Benchmark results:\n\n"
              printf "Random variable generation time = \t %0.9f \n" ((fromIntegral (end2 - start)) / (10^12) :: Double)
              printf "CPU time = \t %0.9f \n" ((fromIntegral (end - start)) / (10^12) :: Double)
              printf "N = \t%d\n" c 
              printf "No. Gaussian pairs = \t%d\n" pairs 
              printf "Sums = \t %0.9f \t%0.9f \n" sumxs sumys 
              printf "Counts : \n"
              -- print results from 0 to 9
              U.forM_ (U.enumFromN 0 10) $ \i -> do 
                let to_p = may_xys BV.! i 
                printf "%d\t %d\n" i to_p 
              return ()
			
		
		
              