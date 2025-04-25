module Uebung01_04 where

charAt :: String -> Int -> Char
charAt [] _ = error "Empty list"
charAt s c
    | c+1 > length s = error "Index too large" 
    | otherwise = last (take (c+1) s)

initialString :: Int -> String -> String
initialString n _ 
  | n <= 0    = []
initialString _ []    = []
initialString n (x:xs)
  | n > 0     = x : initialString (n - 1) xs

subString :: String -> Int -> Int -> String
subString s n m = subString' n (take (n + m) s)
    where subString' :: Int -> String -> String
          subString' i ss
            | i == 0 = ss
            | otherwise = subString' (i - 1) (tail ss)

reverse1 :: [a] -> [a]
reverse1 []      = [] 
reverse1 (x:xs)  = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 x = acc x []
    where acc :: [a] -> [a] -> [a]
          acc []     xss  = xss
          acc (x:xs) xss = acc xs (x:xss) 