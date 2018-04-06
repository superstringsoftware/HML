{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Lib
-- Copyright   : [2018] J X-Ray Ho
-- License     : BSD3
--
-- Maintainer  : J X-Ray Ho <jho.xray@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Lib (

  dotp

) where

import Data.Array.Accelerate

-- | A simple vector inner product
--
dotp :: Acc (Vector Double) -> Acc (Vector Double) -> Acc (Scalar Double)
dotp xs ys = fold (+) 0 ( zipWith (*) xs ys)
