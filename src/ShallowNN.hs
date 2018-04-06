{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Lib
-- Copyright   : [2018] J X-Ray Ho
-- License     : BSD3
--
-- Maintainer  : J X-Ray Ho <jho.xray@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module ShallowNN where

import System.IO
import Data.Array.Accelerate
import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.Numeric.LinearAlgebra

import AccelerateCore.Base

-- very stupid and straightforward FFN definition
data ShallowNet = ShallowNet {
    inodes :: Int,
    hnodes :: Int,
    onodes :: Int,
    learningRate :: Double
} deriving (Show);


-- since we are following the python based "code yourself a neural net" book, using naive matching of their functions
numpyRand :: Int -> Int -> IO (Array DIM2 Double)
numpyRand rows cols = randomArray (uniformR (-0.5 :: Double, 0.5 :: Double)) (Z :. rows :. cols)

testNet0 = ShallowNet 3 3 3 0.3

wih = numpyRand (hnodes testNet0) (inodes testNet0)
who = numpyRand (hnodes testNet0) (inodes testNet0)

