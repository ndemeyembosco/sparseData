{-# LANGUAGE GADTs, FlexibleInstances #-}



module Read where

import SparseData 

-- import Control.Monad.Par 
import Data.Monoid 

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V   
import qualified Data.Vector.Generic as G
-- import qualified Data.Set as S 
import Text.Read
import System.IO  




--- reading matrices in the matrix market format!

readIntoTuple :: (U.Unbox a, Read a) =>  String -> (a, Int, Int)
readIntoTuple s = (read $ last s', read $ head s' :: Int, read $ head $ tail s' :: Int)
            where 
               s' = words s 


readIntoVector ::  String -> ((Int, Int, Int), U.Vector (Double, Int, Int))
readIntoVector s = (sizeInfo $ head data_s, U.fromList $ map (readIntoTuple :: String -> (Double, Int, Int)) $ tail data_s)
            where 
              data_s = dropWhile isHeaderLine $ lines s 
              sizeInfo w = readIntoTuple w :: (Int, Int, Int) 

isHeaderLine :: String -> Bool 
isHeaderLine s = head s == '%' 


readIntoSparseData ::  String -> IO (SparseData COO Double) 
readIntoSparseData filename = do 
          print filename 
          file_str <- readFile filename
          print (map (readIntoTuple :: String -> (Double, Int, Int)) $ tail $ take 1000 $ dropWhile (\s -> head s == '%') $ lines file_str)
          let 
            ((width, height, _), vec) = readIntoVector file_str
          return $ COO vec width height












