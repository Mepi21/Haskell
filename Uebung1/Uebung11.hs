-- ============================================
-- Aufgabe 1: Summenformel (arithmetische Reihe)
-- ============================================
intSum1 :: Int -> Int
intSum1 n = div (n * (n + 1)) 2
-- Verwendet die bekannte Formel: n(n+1)/2
-- div statt (/) da wir mit ganzen Zahlen arbeiten (Int)
-- Beispiel: intSum1 5 = 5 * 6 / 2 = 15

-- ============================================
-- Aufgabe 2: Rekursive Berechnung der Summe
-- ============================================
intSum2 :: Int -> Int
intSum2 0 = 0                   -- Rekursionsbasis: Summe bis 0 ist 0
intSum2 n = n + intSum2 (n - 1)
-- Rekursiv: intSum2 n = n + (n-1) + ... + 1 + 0
-- Beispiel: intSum2 3 = 3 + 2 + 1 + 0 = 6

-- ============================================
-- Aufgabe 3: Akkumulatorbasierte Version (Tail Recursion)
-- ============================================
intSum3 :: Int -> Int
intSum3 n = intSumH n 0        -- startet mit Akkumulator 0
  where
    intSumH :: Int -> Int -> Int
    intSumH 0 acc = acc        -- Rekursionsbasis: gibt akkumuliertes Ergebnis zurück
    intSumH k acc = intSumH (k - 1) (acc + k)
-- Tail-Recursive Version:
-- intSumH 3 0 → intSumH 2 3 → intSumH 1 5 → intSumH 0 6 → 6
-- Vorteil: effizienter Speicherverbrauch durch Akkumulator

