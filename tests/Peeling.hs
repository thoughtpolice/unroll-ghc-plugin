module Main where

import LoopUnrolling.Annotations

main = do
    -- This application of 10 to accumulator_odd should be optimized by GHC into an application of 8
    print $ accumulator_odd 10

accumulator_odd, accumulator_even :: Int -> [Bool]

{-# ANN accumulator_odd (Peel 2) #-}
accumulator_odd n
  | n < 0     = []
  | otherwise = True : accumulator_even (n - 1)

{-# ANN accumulator_even (Peel 4) #-}
accumulator_even n
  | n < 0     = []
  | otherwise = False : accumulator_odd (n - 1)
