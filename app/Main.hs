{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Criterion.Main
import Data.Vector.Unboxed as U

import System.Random.Mersenne
import qualified Data.Vector.Random.Mersenne as G

randomVec1 :: Int -> IO (U.Vector Double)
randomVec1 n = do
  g <- newMTGen Nothing
  a <- G.random g n :: IO (U.Vector Double)
  return a

setupEnv = do
  gen <- newMTGen Nothing
  small <- G.random gen 10000000 :: IO (U.Vector Double)
  big <- G.random gen 10 :: IO (U.Vector Double)
  return (small, big)

main =
  defaultMain [
     -- notice the lazy pattern match here!
     env setupEnv $ \ ~(small,big) -> bgroup "main" [
     bgroup "small" [
       bench "foldl" $ nf (dotV1 small) small
     , bench "foldl'" $ nf (dotV2 small) small
     -- , bench "foldr" $ nf (dotV3 small) small
     , bench "foldr'" $ nf (dotV4 small) small
     , bench "sum" $ nf (dotV5 small) small
     , bench "foldl no zip" $ nf (dotV6 small) small
     , bench "foldl' no zip" $ nf (dotV7 small) small
     -- , bench "foldM' no zip" $ whnf (dotV8 small) small
     ]
  ] ]
