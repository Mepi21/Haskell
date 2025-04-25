-- Aufgabe 1

-- Fakultät (Hilfsfunktion)
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)                               -- n * (n-1)!

-- Binominalkoeffizient
binom :: Integer -> Integer -> Integer
binom n 0 = 1                                       --(n über 0) = 1
binom n k | k == n = 1                              --(n über n) = 1
binom n k | k < 0 || k > n = 0                      --(ungültige Eingaben) = 0
binom n k = div (fac n )(fac k * fac (n - k))       --binom n k = fac n / (fac k * (fac n-k))


-- Aufgabe 2
-- Pascalsches Dreieck
pascal :: Integer -> Integer -> Integer
pascal row 0 = 1                                                    -- Anfang jeder Reihe ist 1
pascal row pos | row == pos = 1                                     -- Ende jeder Reihe ist 1
pascal row pos | row < pos = 0                                      -- Wenn die Position > Reihe ist, gibt es das Element nicht (Ausgabe = 0)
pascal row pos = pascal (row - 1) (pos - 1) +  pascal (row - 1) pos -- Summe der beiden links und rechts darüberstehenden Zahlen


