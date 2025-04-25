intSqrt :: Int -> Int
intSqrt n = maximum [x | x <- [0..n], x * x <= n]

approachSqrt :: Float -> Float -> Float
approachSqrt x epsilon = bisect 0 x
  where
    bisect low high
      | abs (mid * mid - x) <= epsilon = mid
      | mid * mid < x  = bisect mid high
      | otherwise      = bisect low mid
      where
        mid = (low + high) / 2