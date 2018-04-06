module HML.Base.Algebra
    where

import System.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Vector.Generic as G
import System.Random.Mersenne
import qualified Data.Vector.Random.Mersenne as G
import GHC.Prim
import Prelude hiding ((!))


type DVector = U.Vector Double
type DMatrix = V.Vector DVector

randomVec :: Int -> IO (U.Vector Double)
randomVec n = do
  g <- newMTGen Nothing
  a <- G.random g n :: IO (U.Vector Double)
  return a

-- dot product of 2 vectors, resulting in a scalar
dotV :: DVector -> DVector -> Double
dotV v1 v2 = U.sum $ U.zipWith (*) v1 v2

-- multiply matrix by a vector, get a vector
dotMV :: DMatrix -> DVector -> DVector
dotMV m v = U.generate (V.length m) $ \i -> (m ! i) `dotV` v

v = U.fromList [1,2,3,4] :: DVector
r1 = U.fromList [-1,2.3,1.1,-4.3] :: DVector
r2 = U.fromList [2.3,1.8,-3.4,5] :: DVector

m = V.fromList [r1,r2] :: DMatrix
