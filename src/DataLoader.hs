{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}

module DataLoader where 

import MMParser 
import qualified Data.Map.Strict as M 
import qualified Data.Vector.Unboxed as U 
import System.Directory 
import Control.Monad 
import Data.Maybe 

type MatrixData = M.Map String [MMExchange] 


load_data :: FilePath -> IO MatrixData
load_data dir  = do 
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




matrix_data :: M.Map String MMExchange -> M.Map String MMExchange -> IO (M.Map String MMExchange, M.Map String MMExchange)
matrix_data small_ms big_ms  = do 

        big_mdata <- load_data "/home/users/ndemeye/Documents/nasHSpar/src/bigger_matrices"
        let Just (head -> rdb200)       =  M.lookup "rdb200" big_mdata
        let Just (head -> rdb450)    =  M.lookup "rdb450" big_mdata
        let Just (head -> rdb800l)       =  M.lookup "rdb800l" big_mdata
        let m6 = M.insert "rdb200" rdb200 big_ms
        let m7 = M.insert "rdb450" rdb450 m6
        let m8 = M.insert "rdb800l" rdb800l m7 
        let Just (head -> bcsstk08)  = M.lookup "bcsstk08" big_mdata
        let Just (head -> bcsstk09)  = M.lookup "bcsstk09" big_mdata
        let Just (head -> bcsstk09)  = M.lookup "bcsstk09" big_mdata
        let Just (head -> bcsstk10)  = M.lookup "bcsstk10" big_mdata
        let Just (head -> bcsstk11)  = M.lookup "bcsstk11" big_mdata
        let Just (head -> bcsstk12)  = M.lookup "bcsstk12" big_mdata
        let Just (head -> bcsstk13)  = M.lookup "bcsstk13" big_mdata
        let Just (head -> bcsstk14)  = M.lookup "bcsstk14" big_mdata
        let Just (head -> bcsstk15)  = M.lookup "bcsstk15" big_mdata
        let Just (head -> bcsstk16)  = M.lookup "bcsstk16" big_mdata
        let Just (head -> bcsstk17)  = M.lookup "bcsstk17" big_mdata
        let Just (head -> bcsstk18)  = M.lookup "bcsstk18" big_mdata
        let m1 = M.insert "bcsstk08" bcsstk08 big_ms 
        let m2 = M.insert "bcsstk09" bcsstk09 m1
        let m3 = M.insert "bcsstk10" bcsstk10 m2 
        let m4 = M.insert "bcsstk11" bcsstk11 m3
        let m5 = M.insert "bcsstk12" bcsstk12 m4 
        let m6 = M.insert "bcsstk13" bcsstk13 m5
        let m7 = M.insert "bcsstk14" bcsstk14 m6
        let m8 = M.insert "bcsstk15" bcsstk15 m7
        let m9 = M.insert "bcsstk16" bcsstk16 m8
        let m10 = M.insert "bcsstk17" bcsstk17 m9
        let m11 = M.insert "bcsstk18" bcsstk18 m10 
        return (M.empty, m11) 
