module FCM where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Random
import qualified Data.Vector as V
import qualified Data.Matrix as M

type DoublesVector = V.Vector Double
type DoublesMatrix = M.Matrix Double
type DoublesVectorsDistance = DoublesVector -> DoublesVector -> Double

-------------------------------------------------------------------

hammingDistance :: DoublesVectorsDistance
hammingDistance v1 v2 = sum $ V.zipWith (\x1 x2 -> abs (x1 - x2)) v1 v2

euclidDistance :: DoublesVectorsDistance
euclidDistance v1 v2 = sqrt . sum $ V.zipWith (\x1 x2 -> (x1 - x2) ^^ 2) v1 v2

-------------------------------------------------------------------

matrixNorm :: DoublesMatrix -> Double
matrixNorm dm = maximum . map abs $ M.toList dm

-------------------------------------------------------------------

randRow :: RandomGen g => Int -> Rand g [Double]
randRow c = replicateM c getRandom

randMatrix :: RandomGen g => Int -> Int -> Rand g [[Double]]
randMatrix n c = replicateM n (randRow c)

normalizedRow :: [Double] -> [Double]
normalizedRow l = map (/ lSum) posL
    where posL = map abs l
          lSum = sum l
    
normalizedMatrix :: [[Double]] -> [[Double]]
normalizedMatrix m = map (normalizedRow) m

randClustersCenters :: RandomGen g => DoublesMatrix -> Int -> Rand g [[Double]]
randClustersCenters xs c =
    let m = M.ncols xs
        xsList = M.toList xs
        minX = minimum xsList
        maxX = maximum xsList
    in replicateM c . replicateM m $ getRandomR (minX, maxX)

-------------------------------------------------------------------
  
vectorsUnderSum :: [Double] -> [DoublesVector] -> [DoublesVector]
vectorsUnderSum uColM xsRows = zipWith (\uEl xsRow -> V.map (* uEl) xsRow) uColM xsRows 

vecSum :: [DoublesVector] -> DoublesVector
vecSum vl = foldl1 (V.zipWith (+)) vl

clustersCenters :: Double -> DoublesMatrix -> DoublesMatrix -> DoublesMatrix
clustersCenters m xs u =
   let xsRows = map (V.fromList) (M.toLists xs)
   in M.fromLists [ let uColM = map (** m) uCol
                        uColMSum = sum uColM
                    in V.toList (V.map (/uColMSum) (vecSum $ vectorsUnderSum uColM xsRows) )
                  | uCol <- M.toLists $ M.transpose u
                  ]
                    
-------------------------------------------------------------------

partitionMatrix :: Double -> DoublesVectorsDistance -> DoublesMatrix -> DoublesMatrix -> DoublesMatrix
partitionMatrix m distance xs v =
    let n = M.nrows xs
        c = M.nrows v
    in  M.matrix n c (\(i, k) -> let xi = M.getRow i xs
                                 in 1 / sum [(distance xi (M.getRow k v) / distance xi vj) ** (2 / (m - 1))
                                            | vj <- map (V.fromList) (M.toLists v)
                                            ])  

nextPartitionMatrix :: Double -> DoublesVectorsDistance -> DoublesMatrix -> DoublesMatrix -> DoublesMatrix
nextPartitionMatrix m distance xs u = partitionMatrix m distance xs $ clustersCenters m xs u

-------------------------------------------------------------------

fcm :: Double -> Double -> DoublesVectorsDistance -> DoublesMatrix -> DoublesMatrix -> DoublesMatrix
fcm m e distance xs u =
    let uList = iterate (nextPartitionMatrix m distance xs) u
    in snd $ fromJust $ find (\(u, nextU) -> matrixNorm (nextU - u) < e) $ zip uList $ tail uList
    