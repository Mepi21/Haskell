-- Ganzzahlige Wurzelfunktion
intSqrt :: Int -> Int
intSqrt n = search 0
  where
    search :: Int -> Int                --Hilfsfunktion search
    search x
      | x * x > n     = x - 1           --Wenn das X im Quadrat bereits größer als die gegeben Zahl ist (x-1 ausgeben)
      | otherwise     = search (x + 1)  -- Ansonsten X immer weiter rekursiv erhöhen 




-- Näherungsfunktion für Gleitkommazahlen
approachSqrt :: Float -> Float -> Float
approachSqrt n eps = bisect 0 n                         -- Starte die Suche im Intervall [0, n]
  where
    average :: Float -> Float -> Float                  -- Lokale Hilfsfunktion zum Berechnen des Mittelwertes
    average x y = (x + y) / 2

    bisect :: Float -> Float -> Float                   -- Lokale Hilfsfunktion für Intervallhalbierung (rekursive Suche)
    bisect low high
      | abs (mid * mid - n) <= eps = mid                -- Abbruchbedingung: mid² nahe genug an n
      | mid * mid < n              = bisect mid high    -- Wurzel liegt im rechten Intervall
      | otherwise                  = bisect low mid     -- Wurzel liegt im linken Intervall
      where
        mid = average low high                          -- Mittelwert des aktuellen Intervalls
