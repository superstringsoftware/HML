module Lib
    where

import System.Random
import Data.Vector.Unboxed as U
import System.Random.Mersenne
import qualified Data.Vector.Random.Mersenne as G
import GHC.Prim

randomVec :: Int -> IO (U.Vector Double)
randomVec n = do
  g <- newMTGen Nothing
  a <- G.random g n :: IO (U.Vector Double)
  return a

-- different implementations of dot product of 2 vectors
dotV1 :: U.Vector Double -> U.Vector Double -> Double
dotV1 v1 v2 = U.foldl (+) 0 $ U.zipWith (*) v1 v2

dotV2 :: U.Vector Double -> U.Vector Double -> Double
dotV2 v1 v2 = U.foldl' (+) 0 $ U.zipWith (*) v1 v2

dotV3 :: U.Vector Double -> U.Vector Double -> Double
dotV3 v1 v2 = U.foldr (+) 0 $ U.zipWith (*) v1 v2

dotV4 :: U.Vector Double -> U.Vector Double -> Double
dotV4 v1 v2 = U.foldr' (+) 0 $ U.zipWith (*) v1 v2

dotV5 :: U.Vector Double -> U.Vector Double -> Double
dotV5 v1 v2 = U.sum $ U.zipWith (*) v1 v2

dotV6 :: U.Vector Double -> U.Vector Double -> Double
dotV6 v1 v2 = U.ifoldl func 0 v2
  where func acc i el = acc + el *  (v1 U.! i)

dotV7 :: U.Vector Double -> U.Vector Double -> Double
dotV7 v1 v2 = U.ifoldl' func 0 v2
  where func acc i el = acc + el * (v1 U.! i)

dotV8 :: U.Vector Double -> U.Vector Double -> IO Double
dotV8 v1 v2 = U.ifoldM' func 0 v2
  where func acc i el = pure $ acc + el * (v1 U.! i)

dotV9 :: U.Vector Double -> U.Vector Double -> IO Double
dotV9 v1 v2 = U.ifoldM' func 0 v2
  where func acc i el = pure $ acc + el * (v1 U.! i)

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x)) * 3
