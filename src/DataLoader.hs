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
                                fcontents   <- readFile filename
                                case myparse parseMMExchange fcontents of 
                                     Right mdata -> return mdata
                                     Left err    -> return Empty
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




matrix_data :: M.Map String MMExchange -> IO (M.Map String MMExchange)
matrix_data m  = do 
        mdata <- load_data "/home/users/ndemeye/Documents/nasHSpar/src/test_matrices"
        let Just (head -> tub100_data)  =  M.lookup "tub100" mdata
        let Just (head -> pores_1_data) =  M.lookup "pores_1" mdata
        let Just (head -> lf10_data)    =  M.lookup "LF10" mdata
        let Just (head -> bcsstm03)     =  M.lookup "bcsstm03" mdata
        let Just (head -> bcsstk03)     =  M.lookup "bcsstk03" mdata
        let m1 = M.insert "tub100" tub100_data m 
        let m2 = M.insert "pores_1_data" pores_1_data m1
        let m3 = M.insert "LF10_data" lf10_data m2 
        let m4 = M.insert "bcsstm03" bcsstm03 m3 
        let m5 = M.insert "bcsstk03" bcsstk03 m4 
        return m5 
