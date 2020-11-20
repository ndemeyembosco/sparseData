{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}

module DataLoader where 

import MMParser 
import qualified Data.Map.Strict as M 
import qualified Data.Vector.Unboxed as U 
import qualified Data.Vector as B 
import System.Directory 
import Control.Monad 
import Data.Maybe 

type MatrixData = M.Map String [MMExchange] 


load_data :: FilePath -> IO MatrixData
load_data dir  = do 
   print dir 
   mat_dirs <- listDirectory dir
   matrices <- forM mat_dirs $ \n -> do 
                if n == ".DS_Store" then return []
                else do 
                   let dirname = ((dir ++ "/") ++ n)
                   dfiles  <- listDirectory dirname
                   mats    <- forM dfiles $ \f -> do
                                let filename = ((dirname ++ "/") ++ f)
                                print filename 
                                fcontents   <- readFile filename
                                case myparse parseMMExchange fcontents of 
                                     Right mdata -> return mdata
                                     Left err    -> error "returned empty"
                   return mats
   return $ M.fromList $ zip mat_dirs matrices


-- vectors to dot with mats_with_doubles
gen_vec :: Int -> U.Vector Double
gen_vec n = U.replicate n 1.0  

gen_vec_boxed :: Int -> B.Vector Double 
gen_vec_boxed n = B.replicate n 1.0 

mats_with_doubles = [ "tub100"
                    , "pores_1"
                    , "space_station_1"
                    , "nose4"
                    , "Maragal_1"
                    , "LF10"
                    , "Hamrle1"
                    , "cage3"
                    , "bcsstm03"
                    , "bcsstk03"]

get_data_with_double :: [String] -> MatrixData -> [Maybe [MMExchange]]
get_data_with_double l dict = map (\s -> M.lookup s dict) l 




matrix_data' :: M.Map String MMExchange -> M.Map String MMExchange -> IO (M.Map String MMExchange)
matrix_data' small_ms big_ms  = do 

        big_mdata <- load_data "dist-newstyle/resources/bigger_matrices"
        let Just (head -> asic_680k)   = M.lookup "ASIC_680k" big_mdata
        let Just (head -> com_orkut)   = M.lookup "com-Orkut" big_mdata 
        let Just (head -> ecology1)    = M.lookup "ecology1" big_mdata 
        let Just (head -> europe_osm)  = M.lookup "europe_osm" big_mdata 
        let Just (head -> rajat21)     = M.lookup "rajat21" big_mdata
        let m1 = M.insert "asic_680k" asic_680k big_ms 
        let m2 = M.insert "com_orkut" com_orkut m1 
        let m3 = M.insert "ecology1" ecology1 m2 
        let m4 = M.insert "europe_osm" europe_osm m3 
        let m5 = M.insert "rajat21" rajat21 m4 
        return m5  


vec_data :: M.Map String (B.Vector Double)
vec_data = M.fromList [
            ("asic_680k"  , gen_vec_boxed 682862)
         ,  ("com_orkut"  , gen_vec_boxed 3072441)
         ,  ("ecology1"   , gen_vec_boxed 1000000)
         ,  ("europe_osm" , gen_vec_boxed 50912018)
         ,  ("rajat21"    , gen_vec_boxed 411676)
         ]
