{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}


module SparseData where 

import qualified Data.Vector.Mutable as VB 
import qualified Data.Vector as VU 
import qualified Data.Vector.Unboxed as U 
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Generic as G 
import qualified Control.Monad.Par as P 
-- import qualified Control.Monad.Par.IO as P (ParIO, runParIO, IVar)
import Control.Monad.Par.Class  
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Primitive
import Control.Monad.IO.Class (liftIO)
import Control.Monad 
import Control.Monad.ST (runST)
import Patterns (parDivConqGenV, parDivConqZipsGenV)
import qualified Data.Map as M  
            

class U.Unbox a => Sparse rep a where 
  data SparseData rep a :: * 
  smap                  :: (U.Unbox b, NFData a, NFData b, Ord b) => (a -> b) -> SparseData rep a -> SparseData rep b  
  szipWith              :: (U.Unbox b, U.Unbox c, Num a, Num b, Num c, Eq c) => (a -> b -> c) -> SparseData rep a -> SparseData rep b  -> SparseData rep c  
  (#.)                  :: (Num a) => SparseData rep a  -> U.Vector a ->  U.Vector a 
  dims                  :: (Num a) => SparseData rep a  -> (Int, Int)
  is_null               :: SparseData rep a -> Bool  
  



-- the coordinate representation 
data COO 

instance U.Unbox a => Sparse COO a where 
  data instance SparseData COO a  = COO { coo_vals :: U.Vector (a, Int, Int), width :: Int, height :: Int}

  smap !f !v = let 
                  vec  = coo_vals v 
                  w    = width v 
                  h    = height v   
               in v { coo_vals = U.map (\(x, i, j) -> (f x, i, j)) vec, width=w, height=h} 
  szipWith !func !v1 !v2 = if not ((h1 == h2) && (w1 == w2))
                           then error "cannot zip matrices of different dimentions" 
                           else 
                           if and [U.null vec1, U.null vec2] then 
                                v1{coo_vals = U.empty}
                           else if or [U.null vec1, U.null vec2] then
                                error "one vector has non non-zero elements!" 
                                else COO ((U.map (\(!x, !i, !j) -> app_func func x i j vec2) vec1) U.++ (filterZeros $ U.map (\(y, w, z) -> func_app func y w z vec1) vec2)) w1 h1    
                  where  
                      vec1 = coo_vals v1
                      vec2 = coo_vals v2 
                      h1   = height v1 
                      h2   = height v2 
                      w1   = width v1 
                      w2   = width v2 
                      app_func !f !a !i !j !vec = app_func' f a i j $ U.find (\(w, x, y) -> x == i && j == y) vec 
                      app_func' !f r i j Nothing           = (f r 0, i, j) 
                      app_func' !f r i j (Just (!t, _, _)) = (f r t, i, j) 

                      func_app f !a !i !j !vec = func_app' f a i j $ U.find (\(w, x, y) -> x == i && j == y) vec 
                      func_app' !f a i j Nothing   = (f 0 a, i, j)
                      func_app' !f a i j (Just _)  = (0, i, j) 

                      filterZeros vec = U.filter (\(k, _, _) -> k /= 0) vec 



  (#.) !mat !vec  = U.imap (\(!i) (!x) -> U.sum $! U.map (\(!a, _, _) -> a*x) $! colums i mat) vec 
                where
                  colums !i !m = U.filter (\(!a, _, !n) -> n == i) $! coo_vals m 
  dims mat                    = (width mat, height mat)
  is_null COO{coo_vals=v}     = U.null v 


            
-- -- the compressed sparse row representation

data CSR 

instance U.Unbox a => Sparse CSR a where 
  data instance SparseData CSR a = CSR { row_offsets :: U.Vector Int
                                       ,  col_index_csr :: U.Vector Int
                                       ,  csr_vals :: U.Vector a
                                       ,  csr_height :: !Int
                                       ,  csr_width :: !Int
                                       } 
  smap f v = let vec = csr_vals v in v {csr_vals = U.map f vec}
  szipWith !func !v1 !v2 = undefined   
                                                                                                
  (#.) !mat@CSR{ row_offsets=row_off
               , col_index_csr=col
               , csr_vals=val
               } 
               !vec 
              = if U.null val then error "trying to multiply with empty matrix" 
                else U.imap (\i _ -> U.sum $ mult_val_vec i) (U.replicate (U.length vec) n_val) 
        where 
            n_val = U.head val * 0     -- only so the type checker is happy
            get_vals_from_offsets i  = let ri = row_off U.! i in U.slice ri (row_off U.! (i + 1) - ri) val  
            get_cols_from_offsets = \i -> let ri = row_off U.! i in U.slice ri (row_off U.! (i + 1) - ri) col  
            mult_val_vec i = U.zipWith (\v c -> v * vec U.! c) (get_vals_from_offsets i) (get_cols_from_offsets i)
  dims mat = (csr_width mat, csr_height mat)
  is_null CSR{csr_vals=v} = U.null v 




--- The ELL format 

data ELL 

instance U.Unbox a => Sparse ELL a where 
    data instance SparseData ELL a = ELL { max_elem_row :: !Int, col_index_ell :: U.Vector Int, ell_vals :: U.Vector a, ell_height :: !Int, ell_width :: !Int}
    smap f v            = let val_vec = ell_vals v in v {ell_vals = mapParN 1 f val_vec}
    szipWith func v1@ELL{max_elem_row=mr1
                       , ell_vals=vals1 
                       , ell_height=height1
                       , col_index_ell=col_index1} 
                       v2@ELL{
                           max_elem_row=mr2 
                        ,  ell_vals=vals2 
                        ,  ell_height=height2
                        , col_index_ell=col_index2} = ELL max_mr c_index (vals_answer bigger smaller max_mr min_mr) height1 height1
                            where 
                                max_mr = max mr1 mr2 
                                min_mr = min mr1 mr2 
                                c_index = if max_mr == mr1 then col_index1 else col_index2
                                vals_answer b s mb ms = case (b, s) of 
                                    (Left bigger, Right smaller) -> U.imap (\i x -> case (smaller U.!? (other_index i mb ms)) of 
                                        Just y -> func x y
                                        Nothing -> func x 0) bigger
                                    (Right bigger, Left smaller) -> U.imap (\i x -> case (smaller U.!? (other_index i mb ms)) of 
                                        Just y -> func y x 
                                        Nothing -> func 0 x) bigger  
                                (bigger, smaller) = if mr1 > mr2 then (Left vals1, Right vals2) else (Right vals2, Left vals1)  
                                other_index i mr other_mr = let (row, col) = i `divMod` mr in row * other_mr + col 
    (#.) mat@ELL{max_elem_row=mr
                     , ell_vals=vals
                     , ell_height=height
                     , col_index_ell=col_index
                       } vec        = if U.null vec 
                                          then U.empty 
                                          else 
                                            U.ifoldr (\i (prod, row) to_return -> update_if_row i prod row to_return) 
                                             (U.replicate (U.length vec) 0) 
                                             $ U.izipWith col_mult col_index vals 
                where
                    col_mult i col val = let 
                                            x_val = vec U.! col 
                                            row   = i `mod` mr 
                                         in (x_val * val, row) 
                    update_if_row  i p r vec = if i == r
                                                then 
                                                   let 
                                                     curr_val = vec U.! i 
                                                   in vec U.// [(i, curr_val + p)]
                                                else vec
    dims mat = (ell_width mat, ell_height mat)
    is_null ELL{ell_vals=v} = U.null v 


countFreqs' :: U.Vector Int  -> M.Map Int Int  
countFreqs' v = U.foldr (\elem dict -> case M.lookup elem dict of 
                              Nothing -> M.insert elem 1 dict 
                              Just n  -> M.insert elem (n + 1) dict) M.empty v   

countFreqs'' :: U.Vector Int -> U.Vector Int -> U.Vector Int 
countFreqs'' vec helper = let m = M.toList $ countFreqs' vec in helper U.// m  

countFreqs ::  Int -> U.Vector Int -> U.Vector Int 
countFreqs len vec = countFreqs'' vec (U.replicate len 0)

                          

---- conversions
coo_to_csr :: U.Unbox a => SparseData COO a -> SparseData CSR a 
coo_to_csr COO{coo_vals=vec, width=w, height=h} = CSR {csr_height=h
                                                         , csr_width=w
                                                         , row_offsets=row_offs 
                                                         , col_index_csr=col_index 
                                                         , csr_vals=vals}
                                    where
                                        (vals, rows, col_index) = U.unzip3 vec 
                                        row_offs = if U.null vals then U.singleton 0 else U.prescanl (+) 0 $ countFreqs (h + 1) rows 





csr_to_coo :: U.Unbox a => SparseData CSR a -> SparseData COO a 
csr_to_coo CSR{csr_height=h, csr_width=w
            , row_offsets=row_offs, col_index_csr=col_index
            , csr_vals=vals} = COO{coo_vals=vec, width=w, height=h} 
            where
                vec = U.zip3 vals (fromOffsets row_offs) col_index 
                fromOffsets l = U.concatMap (\(i, j) -> if j /= 0 then U.replicate j (j - 1) else U.empty) $ U.zip l ((U.tail l) U.++ (U.fromList [0]))

-- coo_to_ell :: U.Unbox a => SparseData COO a -> SparseData ELL a 
-- coo_to_ell m@COO{coo_vals=vals, width=w, height=h} = let (e_vals, e_cols) = vals_to_ell vals in ELL my_mr e_cols e_vals h w 
--                                          where
--                                             (vs, rs, cs) = U.unzip3 vals  


-- ell_to_coo :: U.Unbox a => SparseData ELL a -> SparseData COO a 
-- ell_to_coo = undefined
      
-- ----------------------------------------------------------------------------------------------------------------------------
-- ------------------------------------- Generic operations & algorithms (any representation) ---------------------------------
-- ----------------------------------------------------------------------------------------------------------------------------


scale   :: (U.Unbox a, Num a, Ord a, Sparse rep a, NFData a) => a -> SparseData rep a  -> SparseData rep a  
scale x = smap (*x)


(#+) :: (Sparse rep a, Num a, U.Unbox a, Eq a) => SparseData rep a  -> SparseData rep a  -> SparseData rep a  
(#+) = szipWith (+)  

(#-) :: (Sparse rep a, Num a, U.Unbox a, Eq a) => SparseData rep a  -> SparseData rep a  -> SparseData rep a 
(#-) = szipWith (-)


------------------------ Generic Iterative linear solvers --------------------------------------------------------------------------

cg :: (Num a, Sparse rep a, U.Unbox a, Eq a, Floating a) => Int -> U.Vector a -> SparseData rep a ->  U.Vector a -> (a, U.Vector a)
{-# INLINE cg #-}
cg !iters !z !a !x = 
    let 
        !r     = x 
        !p     = r  
        !rho   = r <.> r 
    in go a z x rho p r 0 
    where
        {-# INLINE go #-}
        go !a !z !x !d !p !r !i | i == iters     =  
                                    let 
                                        !residual = (a #. z) ^-^ x 
                                        !to_ret = sqrt $ residual <.> residual 
                                    in (to_ret, z) 
                                | otherwise =  
                                    let 
                                        !q         = a #. p 
                                        !alpha     = d / (p <.> q) 
                                        !new_z     = z ^+^ (alpha .* p) 
                                        !new_r     = r ^-^ (alpha .* q)  
                                        !new_d     = new_r <.> new_r 
                                        !beta      = new_d / d  
                                        !new_p     = new_r ^+^ (beta .* p) 
                                    in go a new_z x new_d new_p new_r (i + 1)
        (<.>) v1 v2   = U.sum $ U.zipWith (*) v1 v2 
        (^+^)         = U.zipWith (+)
        (^-^)         = U.zipWith (-)  
        (.*) c        = U.map (*c)  


---------------------------------------------------------------------------------------------------------------------------------------







