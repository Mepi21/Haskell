module Uebung01_01 where

intSum1 :: Int -> Int
intSum1 n = div(n^2 + n) 2

intSum2 :: Int -> Int
intSum2 0 = 0
intSum2 n = n + intSum2(n - 1)

intSum3 :: Int -> Int
intSum3 = intSum3' 0
    where   intSum3' :: Int -> Int -> Int
            intSum3' c 0  = c
            intSum3' c n  = intSum3' (c + n) (n - 1)