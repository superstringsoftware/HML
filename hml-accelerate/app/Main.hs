{-# LANGUAGE CPP #-}
-- |
-- Module      : Main
-- Copyright   : [2018] J X-Ray Ho
-- License     : BSD3
--
-- Maintainer  : J X-Ray Ho <jho.xray@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Lib
import ShallowNN
import AccelerateCore.Base

import Text.Printf
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import Data.Array.Accelerate.LLVM.Native                            as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import Data.Array.Accelerate.LLVM.PTX                               as PTX
#endif

import Data.Array.Accelerate.System.Random.MWC

type Matrix = Array DIM2

main :: IO ()
main = do
  let
      xs :: Vector Double
      xs = fromList (Z:.10) [0..]

      ys :: Vector Double
      ys = fromList (Z:.10) [1,3..]

      zs0 :: IO  (Vector Double) 
      zs0 = randomArray (uniformR (0.0 :: Double,1.0 :: Double)) (Z:.10)

  zs <- zs0

  matr <- numpyRand 2 2

  printf "input data:\n"
  printf "xs = %s\n" (show xs)
  printf "ys = %s\n\n" (show ys)

  printf "zs = %s\n\n" (show zs)

  printf "mx = %s\n\n" (show matr)
  printf "testNet = %s\n\n" (show testNet0)
  
  printf "the function to execute:\n"
  printf "%s\n\n" (show dotp)

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  printf "result with CPU backend: dotp xs ys = %s\n" (show (CPU.runN dotp xs ys))
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  printf "result with PTX backend: dotp xs ys = %s\n" (show (PTX.runN dotp xs ys))
#endif

  testComputeZ


-- ***************** BASIC TESTS **************************
testComputeZ :: IO ()
testComputeZ = do
    let
        a :: Vector NNElement
        a = fromList (Z:.2) [2,3]

        b :: Vector NNElement
        b = fromList (Z:.2) [5,3]
  
        w :: Matrix NNElement
        w = fromList (Z:.2:.2) [1,2,4,3]

    printf "input data:\n"
    printf "a = %s\n" (show a)
    printf "b = %s\n\n" (show b)
    
    printf "w = %s\n\n" (show w)
    
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
    printf "result with CPU backend: computeZ w a b = %s\n" (show (CPU.runN computeZ w a b))
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
    printf "result with PTX backend: computeZ w a b = %s\n" (show (PTX.runN computeZ w a b))
#endif
