{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE BangPatterns, DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module SparseData where 

import qualified Data.Vector.Unboxed as U 
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Generic as G 
import Control.Monad.Par
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive
import Control.Monad 
import Patterns (parDivConqGenV, parDivConqZipsGenV)


{-# INLINE mapParN #-}
mapParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (a -> b) -> U.Vector a -> U.Vector b
mapParN !n !f !v = parDivConqGenV v n (U.map f) (U.++)


{-# INLINE imapParN #-}
imapParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (Int -> a -> b) -> U.Vector a -> U.Vector b
imapParN !n !f !v = parDivConqGenV v n (U.imap f) (U.++) 


{-# INLINE filterParN #-}
filterParN :: (NFData a, U.Unbox a, Ord a) => Int -> (a -> Bool) -> U.Vector a -> U.Vector a
filterParN !n !f !v = parDivConqGenV v n (U.filter f) (U.++)


{-# INLINE ifilterParN #-}
ifilterParN :: (NFData a, U.Unbox a, Ord a) => Int -> (Int -> a -> Bool) -> U.Vector a -> U.Vector a
ifilterParN !n !f !v = parDivConqGenV v n (U.ifilter f) merge

{-# INLINE mapMaybeParN #-}
mapMaybeParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (a -> Maybe b) -> U.Vector a -> U.Vector b
mapMaybeParN !n !f !v = parDivConqGenV v n (U.mapMaybe f) merge


{-# INLINE imapMaybeParN #-}
imapMaybeParN :: (NFData a, NFData b, U.Unbox a, U.Unbox b, Ord b) => Int -> (Int -> a -> Maybe b) -> U.Vector a -> U.Vector b
imapMaybeParN !n !f !v = parDivConqGenV v n (U.imapMaybe f) merge


{-# INLINE zipWithParN #-}
zipWithParN :: (NFData a, NFData b, NFData c 
              , U.Unbox a, U.Unbox b, U.Unbox c
              , Ord c) 
            => Int -> (a -> b -> c) 
            -> U.Vector a -> U.Vector b -> U.Vector c 
zipWithParN par_n f v1 v2 = imapParN par_n (\(!i) (!x) -> app_func f x i v2) v1 
                      where 
                        app_func !f !x !i !vec = U.head $! imapMaybeParN par_n (\(!j) (!y) -> if i == j then Just $ f x y else Nothing) vec   


-- (U.map (\(!x, !i, !j) -> app_func func x i j vec2) vec1)     -- consider changing!!!!  -- exploit parallelism?
--                   where  
--                       vec1 = values v1
--                       vec2 = values v2 
--                       app_func !f !a !i !j !vec = U.head $!                                 -- consider changing this implementation
--                                                   U.mapMaybe (\(!b, !x, !y) -> if i == x && j == y then Just (f a b, i, j) else Nothing) vec -- exploit parallelism


-- implement all the remaining list combinators



merge :: (U.Unbox a, Ord a) => U.Vector a -> U.Vector a  -> U.Vector a
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
			

-----------------------------------------------------------------------------------------------------------
---------------------------------- Sparse data core -------------------------------------------------------
-----------------------------------------------------------------------------------------------------------


-- data family SparseData rep a 
-- data EllPack
data COO 

class Sparse rep a where 
  data SparseData rep a :: * 
  smap       :: (U.Unbox a, U.Unbox b, NFData a, NFData b, Ord b) => (a -> b) -> SparseData rep a -> SparseData rep b 
  szipWith   :: (U.Unbox a, U.Unbox b, U.Unbox c) => (a -> b -> c) -> SparseData rep a -> SparseData rep b -> SparseData rep c 
  (#.)       :: (U.Unbox a, Num a) => SparseData rep a -> U.Vector a ->  U.Vector a 
  -- (#*)       :: U.Unbox a => SparseData rep a -> SparseData rep a -> SparseData rep a 


-- instance Sparse EllPack a where 
--   data instance SparseData EllPack a = EllPack { _data :: U.Vector a, col_i :: U.Vector Int, r_nonzeros :: Int, rows :: Int}
--   smap f v = v {_data = mapParN (rows v) f (_data v)}
--   szipWith = undefined 
--   (#.)     = undefined 
--   (#*)     = undefined


instance Sparse COO a where 
  data instance SparseData COO a = COO { values :: U.Vector (a, Int, Int)}
  smap f v = let vec = values v in v { values = mapParN (U.length vec) (\(x, i, j) -> (f x, i, j)) vec} 
  szipWith !func !v1 !v2 = COO (U.map (\(!x, !i, !j) -> app_func func x i j vec2) vec1)     -- consider changing!!!!  -- exploit parallelism?
                  where  
                      vec1 = values v1
                      vec2 = values v2 
                      app_func !f !a !i !j !vec = U.head $!                                 -- consider changing this implementation
                                                  U.mapMaybe (\(!b, !x, !y) -> if i == x && j == y then Just (f a b, i, j) else Nothing) vec -- exploit parallelism

  (#.) !mat !vec  =  U.imap (\(!i) (!x) -> U.sum $! U.map (\(!a, _, _) -> a*x) $! colums i mat) vec 
            where
              colums !i !m = U.filter (\(!a, _, !n) -> n == i) $! values m 
  -- (#*)      = undefined



scale   :: (U.Unbox a, Num a, Ord a, Sparse rep a, NFData a) => a -> SparseData rep a -> SparseData rep a
scale x = smap (*x)


(#+) :: (Sparse rep a, Num a, U.Unbox a) => SparseData rep a -> SparseData rep a -> SparseData rep a 
(#+) = szipWith (+)  

(#-) :: (Sparse rep a, Num a, U.Unbox a) => SparseData rep a -> SparseData rep a -> SparseData rep a 
(#-) = szipWith (-)







