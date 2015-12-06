module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import FCM
import qualified Data.Vector as V
import qualified Data.Matrix as M

hammingDistanceTC1Decription = "Hamming's distance between [0, 0, 0] and [0, 0, 0] = 0"
hammingDistanceTC1 :: Assertion
hammingDistanceTC1 = hammingDistance (V.fromList [0, 0, 0]) (V.fromList [0, 0, 0]) @?= 0

hammingDistanceTC2Decription = "Hamming's distance between [1, -8, 0] and [-3, -2, 0] = 10"
hammingDistanceTC2 :: Assertion
hammingDistanceTC2 = hammingDistance (V.fromList [1, -8, 0]) (V.fromList [-3, -2, 0]) @?= 10

hammingDistanceTC3Decription = "Hamming's distance between [1, 0, 0] and [0, 1, 0] = 2"
hammingDistanceTC3 :: Assertion
hammingDistanceTC3 = hammingDistance (V.fromList [1, 0, 0]) (V.fromList [0, 1, 0]) @?= 2

hammingDistanceTC4Decription = "Hamming's distance between [0.5, 0.3] and [0.1, 0.1] = 0.6"
hammingDistanceTC4 :: Assertion
hammingDistanceTC4 = hammingDistance (V.fromList [0.5, 0.3]) (V.fromList [0.1, 0.1]) @?= 0.6

euclidDistanceTC1Decription = "Euclid's distance between [-3, -4] and [0, 0] = 5"
euclidDistanceTC1 :: Assertion
euclidDistanceTC1 = euclidDistance (V.fromList [-3, -4]) (V.fromList [0, 0]) @?= 5

euclidDistanceTC2Decription = "Euclid's distance between [2, 7, 5, 4] and [2, 7, 5, 4] = 0"
euclidDistanceTC2 :: Assertion
euclidDistanceTC2 = euclidDistance (V.fromList [2, 7, 5, 4]) (V.fromList [2, 7, 5, 4]) @?= 0

euclidDistanceTC3Decription = "Euclid's distance between [-2, -2, -2, -2] and [-3, -3, -3, -3] = 2"
euclidDistanceTC3 :: Assertion
euclidDistanceTC3 = euclidDistance (V.fromList [-2, -2, -2, -2]) (V.fromList [-3, -3, -3, -3]) @?= 2

matrixNormTC1Decription = "Matrix norm for ((1 2 3), (4 5 6), (7 8 9)) = 9"
matrixNormTC1 :: Assertion
matrixNormTC1 = matrixNorm (M.fromList 3 3 [1..]) @?= 9

matrixNormTC2Decription = "Matrix norm for ((-6 -1), (2 5), (0 -7)) = 7"
matrixNormTC2 :: Assertion
matrixNormTC2 = matrixNorm (M.fromLists [[-6, -1], [2, 5], [0, -7]]) @?= 7

matrixNormTC3Decription = "Matrix norm for ((0 0), (0 0)) = 0"
matrixNormTC3 :: Assertion
matrixNormTC3 = matrixNorm (M.fromLists [[0, 0], [0, 0]]) @?= 0

clustersCentersTC1Decription = "Clusters's centers for M = 2, X = ((0 0), (4 3), (4 0), (0 3)),  U = ((0 0 1), (0 1 0), (0 0 1), (1 0 0)) == ((0 3), (4 3), (2 0))"
clustersCentersTC1 :: Assertion
clustersCentersTC1 = clustersCenters 3 (M.fromLists [[0, 0], [4, 3], [4, 0], [0, 3]]) (M.fromLists [[0, 0, 1], [0, 1, 0], [0, 0, 1], [1, 0, 0]]) @?= M.fromLists [[0, 3], [4, 3], [2, 0]]

clustersCentersTC2Decription = "Clusters's centers for M = 2, X = ((0 0), (4 3), (4 0), (0 3)),  U = ((0 1), (1 0), (0 1), (1 0)) == ((2 3), (2 0))"
clustersCentersTC2 :: Assertion
clustersCentersTC2 = clustersCenters 2 (M.fromLists [[0, 0], [4, 3], [4, 0], [0, 3]]) (M.fromLists [[0, 1], [1, 0], [0, 1], [1, 0]]) @?= (M.fromLists [[2, 3], [2, 0]])

main :: IO ()
main = defaultMain
  [ testCase hammingDistanceTC1Decription hammingDistanceTC1
  , testCase hammingDistanceTC2Decription hammingDistanceTC2
  , testCase hammingDistanceTC3Decription hammingDistanceTC3
  , testCase hammingDistanceTC4Decription hammingDistanceTC4
  , testCase euclidDistanceTC1Decription euclidDistanceTC1
  , testCase euclidDistanceTC2Decription euclidDistanceTC2
  , testCase euclidDistanceTC3Decription euclidDistanceTC3
  , testCase matrixNormTC1Decription matrixNormTC1
  , testCase matrixNormTC2Decription matrixNormTC2  
  , testCase matrixNormTC3Decription matrixNormTC3
  , testCase clustersCentersTC1Decription clustersCentersTC1 
  , testCase clustersCentersTC2Decription clustersCentersTC2  
  ]
  