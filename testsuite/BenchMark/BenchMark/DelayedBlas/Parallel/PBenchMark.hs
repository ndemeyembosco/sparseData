{-# LANGUAGE DataKinds, BangPatterns, LambdaCase  #-}

module BenchMark.DelayedBlas.Parallel.PBenchMark where 

import BenchMark.DelayedBlas.Parallel.PKernels
import qualified Data.Vector as U 
import qualified Data.Map.Strict as M
import Util.Parser.MMParser ( MMExchange, mm_to_s_data_p ) 
import DelayedBlas.Data.Matrix.Parallel.Generic.Generic
    ( Matrix(MatrixData), RepIndex(U), MatrixException (..) ) 
import BenchMark.DelayedBlas.Parallel.Dense.Big.DENSE 
    (bench_dns_big)
import Data.Maybe
import System.Random.PCG
import Control.DeepSeq (deepseq)
import Control.Monad.ST  
import Control.Exception (catch)







hs :: IO ()
hs = main 

main :: IO ()
main = do
      print "starting dense benchmark"
      bench_dns_big  `catch` \case 
                        InvalidVectorLength   -> putStrLn "fromVector received vector argument with invalid length" 
                        FromVecLengthMismatch -> putStrLn "length of vector in type, and length of vector at runtime do not match!" 
                        NullVector            -> putStrLn "A null vector was given as argument to fromVector. This is not allowed."
      return ()



