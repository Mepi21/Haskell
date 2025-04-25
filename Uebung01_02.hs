module Uebung01_02 where

binom :: Integer -> Integer -> Integer
binom n k = run (fac n) ((fac k) * (fac (n-k)))
    where
    run :: Integer -> Integer -> Integer
    run a b = div(a) b

    fac :: Integer -> Integer
    fac 0 = 1
    fac x = x * fac (x - 1)

pascal :: Integer -> Integer -> Integer
pascal row pos
  | pos == 0 || pos == row = 1
  | otherwise = pascal (row - 1) (pos - 1) + pascal (row - 1) pos