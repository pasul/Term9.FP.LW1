module Main where

import Control.Monad.Random
import Data.CSV.Conduit
import System.Environment
import System.IO
import qualified Data.Vector as V
import qualified Data.Matrix as M
import FCMSettings
import FCM

main :: IO ()
main = do
    args <- getArgs
    case parseSettings args defaultSettings of
        (settings, [inFile]) -> do
            let csvSettings = defCSVSettings { csvSep = separator settings }
            csvData <- readCSVFile csvSettings inFile
            let xs = M.fromLists $
                    filter (not . null) $
                    map (map (\s -> read s :: Double) .
                             (if skipFirstCol settings then tail else id) .
                             (if skipLastCol settings then init else id)) $
                        (if skipHeader settings then tail else id) $ V.toList csvData                  
                vectorsDistance' = case vectorsDistance settings of
                    Hamming -> euclidDistance
                    Euclid -> hammingDistance                        
            gen <- getStdGen            
            let u0 = case initialFCMStep settings of
                    RandomPartitioinMatrix -> u0
                        where u0 = partitionMatrix (m settings) vectorsDistance' xs v0
                              v0 = M.fromLists (evalRand (randClustersCenters xs (clustersCount settings)) gen)
                    RandomClusterCenters -> normRandU
                        where normRandU = M.fromLists $ normalizedMatrix randU
                              randU = evalRand (randMatrix (M.nrows xs) (clustersCount settings)) gen
            let resultU = fcm (m settings) (eps settings) vectorsDistance' xs u0
            case outFile settings of
                Nothing -> print resultU
                Just path -> writeCSVFile csvSettings path WriteMode $ map (map show) $ M.toLists resultU                  
        _ -> error "The arguments you specified are not valid. Please take a look at the ReadMe.txt file."    
        