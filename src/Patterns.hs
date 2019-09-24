{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns, DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Patterns where 

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

mapPar :: NFData b => (a -> b) -> [a] -> Par [b]
mapPar = parMap 

mapParM2 :: (NFData a, NFData c, NFData b) => (a -> Par b) -> (b -> Par c) -> [a] -> Par [c]
mapParM2 f g = parMapM (f >=> g)  


mapParM2' :: (NFData a, NFData c, NFData b) => (a -> Par b) -> (b -> Par c) -> [a] -> Par [c]
mapParM2' f g xs = mapM (spawn . (f >=> g)) xs >>= mapM get 

mapParM2'' :: (NFData a, NFData c, NFData b) => (a -> Par b) -> (b -> Par c) -> [a] -> Par [c]
mapParM2'' f g xs = mapM (\x -> do
                           ivar <- spawn $ f x 
                           c    <- get ivar 
                           spawn $ g c  
                            ) xs >>= mapM get  

{-# INLINE replicatePar #-}
replicatePar :: NFData a => 
            Par a         -- data to be replicated 
            -> Int        -- number of computations to be replicated 
            -> Par [a]    -- resulting parallel computation of results 
replicatePar a n = do 
   ivars <- replicateM n (spawn a)
   mapM get ivars  

{-# INLINE splitVec #-}
splitVec :: U.Unbox a => Int -> U.Vector a -> [U.Vector a]
splitVec = splitVecG


{-# INLINE splitVecG #-}
splitVecG :: G.Vector v a  => Int -> v a -> [v a]
splitVecG !n !v | n == v_length     = [v]
                | otherwise = if null rem_list then map (\start -> G.slice start slice_sz v) s_list 
                              else (map (\start -> G.slice start slice_sz v) s_list) ++ rem_list
            where
            !v_length = G.length v 
            (!slice_sz, !rem) = v_length `divMod` n 
            !s_list   = 0 : (map (*slice_sz) [1..(n - 1)])
            !rem_list = [G.slice ((v_length - rem)) rem v] 


{-# INLINE splitVecG2 #-}
splitVecG2 :: (G.Vector v a, G.Vector v b) => Int -> v a -> v b -> [(v a, v b)] 
splitVecG2 !n !v1 !v2 = zip s1 s2 
                 where
                  !s1 = splitVecG n v1 
                  !s2 = splitVecG n v2 

{-#  INLINE splitVec2 #-}
splitVec2 :: (U.Unbox a, U.Unbox b) => Int -> U.Vector a -> U.Vector b -> [(U.Vector a, U.Vector b)]
splitVec2 = splitVecG2


{-# INLINE splitVecB #-}
splitVecB :: Int -> BU.Vector a -> [BU.Vector a]
splitVecB = splitVecG


{-# INLINE scatter #-}
scatter :: (NFData a, NFData b) 
            => a                       -- collection to scatter 
            -> Int                     -- number of subcomputations 
            -> (Int -> a -> [a])       -- function to split into n subcomputations
            -> (a -> b)                -- function for local computation
            -> Par [b]                 -- resulting list of sub_computations (to be used only in the same instance
                                       -- of runPar with users of the)
scatter !vec !n !splitFunc !f = do 
   !ivars <- replicateM n new 
   let 
      !parts     = splitFunc n vec 
      !comps_ivs = zip parts ivars 
   mapM_ (\(!p, !i) -> fork $ let !ans = f p in do 
      put i ans ) comps_ivs
   mapM get ivars 


{-# INLINE scatterZips #-}
scatterZips :: (NFData a, NFData b, NFData c) 
            => a                                      -- collection to scatter 
            -> b                                      -- collection to scatter 
            -> Int                                    -- number of subcomputations 
            -> (Int -> a -> b -> [(a, b)])            -- function to split into n subcomputations
            -> (a -> b -> c)                          -- function for local computation
            -> Par [c]                                -- resulting list of sub_computations (to be used only in the same instance
                                                      -- of runPar with users of the)
scatterZips !vec1 !vec2 !n !splitFunc !f = do 
   !ivars <- replicateM n new 
   let 
      !parts     = splitFunc n vec1 vec2  
      !comps_ivs = zip parts ivars 
   mapM_ (\(!(!p1, !p2), !i) -> fork $ let !ans = f p1 p2 in do 
      put i ans ) comps_ivs
   mapM get ivars



{-# INLINE scatterParVec #-}
scatterParVec :: (NFData a, V.Unbox a, NFData b) 
                     => U.Vector a                        
                     -> Int                               
                     -> (U.Vector a -> b)                
                     -> Par [b]                                                                             
scatterParVec vec n f = scatter vec n splitVec f 


{-# INLINE gather #-}
gather :: (NFData a)
               => [IVar a] 
               -> ([a] -> a)
               -> Par a 
gather ivars merge = do 
                 as <- mapM get ivars 
                 return $ merge as 


{-# INLINE allGather #-}
allGather :: (NFData a)
           => [IVar a]
           -> Int 
           -> ([a] -> a)
           -> Par [a] 
allGather ivars n f = replicatePar (gather ivars f) n  



{-# INLINE parDivConq #-}
parDivConq :: (NFData a, NFData b, Monoid b) 
               => a                        -- the problem  
               -> Int                      -- number of subdivisions 
               -> (Int -> a -> [a])        -- function to split into subcomputations   
               -> (a -> b)                 -- local solver  
               -> (b -> b -> b)            -- merging function     
               ->  b              
parDivConq !vec !n !splitFunc !local_solver !join = runPar $ do 
         !ans <- scatter vec n splitFunc local_solver
         !to_return <- reduce mempty ans join 
         return to_return


parDivConqZips :: (NFData a, NFData b, NFData c, Monoid c)
               => a                              -- problem 1
               -> b                              -- problem 2
               -> Int                            -- number of subdivisions
               -> (Int -> a -> b -> [(a, b)])    -- function to split into zipped subcomputations
               -> (a -> b -> c)                  -- local solver 
               -> (c -> c -> c)                  -- merging function
               -> c 
parDivConqZips !vec1 !vec2 !n !splitFunc !local_solver !join = runPar $ do 
                      !ans       <- scatterZips vec1 vec2 n splitFunc local_solver
                      !to_return <- reduce mempty ans join 
                      return to_return


{-# INLINE parDivConqGenV #-}
parDivConqGenV :: (NFData a, V.Unbox a, NFData b, Monoid b) 
                     => U.Vector a  
                     -> Int          
                     -> (U.Vector a -> b)  
                     -> (b -> b -> b)           
                     ->  b 
parDivConqGenV !vec !n !local_solve !join = parDivConq vec n splitVec local_solve join 



{-# INLINE parDivConqZipsGenV #-}
parDivConqZipsGenV :: (NFData a, V.Unbox a, V.Unbox b, NFData b, NFData c, Monoid c) 
                     => U.Vector a  
                     -> U.Vector b 
                     -> Int          
                     -> (U.Vector a -> U.Vector b -> c)  
                     -> (c -> c -> c)           
                     ->  c 
parDivConqZipsGenV !vec1 !vec2 !n !local_solve !join = parDivConqZips vec1 vec2 n splitVec2 local_solve join 











{-# INLINE scan #-}
scan :: (NFData a, Monoid a) => a -> [a] -> (a -> a -> a) -> Par [a]
scan start lq f = do
   let 
      (!l, !r)     = splitAt (length lq `div` 2) lq 
      g [] a       = [f start a]
      g l1@(t:ts) a = (f t a) : l1  
   !livar <- spawn $ reduceG [] l g (combine f) 
   !rivar <- spawn $ reduceG [] r g (combine f)
   !lans  <- get livar 
   !rans  <- get rivar 
   return $ combine f lans rans 
   where
      combine _ []     q@(x:_)  = q  
      combine _ xs     []       = xs
      combine f l2 ys  = let v = last l2 in l2 ++ (map (\b -> f v b) ys) 



      
      

{-# INLINE reduce #-}
reduce :: (NFData a, Monoid a) => a -> [a] -> (a -> a -> a) -> Par a 
reduce r l f = reduceG r l (flip const) f 




{-# INLINE reduceG #-}
reduceG :: (NFData a, Monoid b, NFData b) 
         => b                -- starting value
         -> [a]              -- problem list 
         -> (b -> a -> b)    -- local accumulator function
         -> (b -> b -> b)    -- global merging function
         -> Par b 
reduceG !r [] _  _           = return r 
reduceG !r [!x] !f _         = return $ f r x 
reduceG !d !x !f !g          = do 
   let (!l, !r) = splitAt (length x `div` 2) x 
   !livar <- spawn $ reduceG mempty l f g 
   !rivar <- spawn $ reduceG mempty r f g 
   !lans  <- get livar 
   !rans  <- get rivar 
   return $! g lans $! g d rans 

  
{-# INLINE mapReduce #-}
mapReduce :: (NFData a, NFData b, Monoid b) => b -> [a] -> (a -> b) -> (b -> b -> b) -> Par b 
mapReduce d l mf rf = mapReduceG d l mf (flip const) rf   

{-# INLINE mapReduceG #-}
mapReduceG :: (NFData a, NFData b, NFData c, Monoid c) 
           => c                -- starting value 
           -> [a]              -- problem list  
           -> (a -> b)         -- function to map 
           -> (c -> b -> c)    -- local accumulating function
           -> (c -> c -> c)    -- global merging funciton
           -> Par c 
mapReduceG d l mf lrf grf = mapPar mf l >>= \l1 -> reduceG d l1 lrf grf 


-- stencils
type Neighborhood = [Int]

parStencil :: (NFData a, NFData b)
           => a 
           -> BU.Vector a 
           -> Neighborhood 
           -> ([a] -> b)
           -> Par (BU.Vector b)
parStencil def_val vec neigh func = do 
   ivars <- BU.imapM (\i x -> spawnP $! func $! neighbors neigh i x) vec  
   BU.mapM get ivars 
   where 
      len = BU.length vec 
      neighbors offsets i x = x : (map (\j -> 
                                           if (0 <= i + j && (i + j) < len)
                                             then vec BU.! (i + j)
                                             else def_val) offsets)

           
                                                                                    
                                





