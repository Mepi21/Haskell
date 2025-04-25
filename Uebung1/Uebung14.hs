-- ===============================================================
-- Gibt das Zeichen an der angegebenen Position zurück
-- ===============================================================
charAt :: String -> Int -> Char
charAt [] _ = error "Index too large"         -- Fehler: Index zu groß für leere Liste
charAt (x:xs) n
  | n < 0     = error "Negative index"        -- Fehler: negativer Index nicht erlaubt
  | n == 0    = x                             -- Index 0: gib das erste Zeichen zurück
  | otherwise = charAt xs (n - 1)             -- Rekursiver Aufruf mit Rest der Liste

-- ===============================================================
-- Gibt die ersten n Zeichen des Strings zurück
-- Verhält sich wie take, auch bei n > Länge oder n <= 0
-- ===============================================================
initialString :: Int -> String -> String
initialString _ []     = []                             -- Wenn String leer: gib leeren String zurück
initialString n _ | n <= 0 = []                         -- Wenn n <= 0: gib leeren String zurück
initialString n (x:xs) = x : initialString (n - 1) xs   -- Nimm erstes Zeichen, rufe rekursiv mit n-1 auf dem Rest auf


-- ===============================================================
-- Gibt einen Teilstring zurück, ab der Startposition 'start' mit Länge 'len'
-- ===============================================================
subString :: String -> Int -> Int -> String
subString str start len = initialString len (dropChars start str)
  -- Nimm zuerst alle Zeichen ab 'start' (dropChars), dann die ersten 'len' Zeichen (initialString)

  where
    -- Lokale Hilfsfunktion drop: überspringt die ersten 'n' Zeichen
    dropChars :: Int -> String -> String
    dropChars _ [] = []                       -- Wenn Liste leer: gib leeren String zurück
    dropChars n xs | n <= 0 = xs              -- Wenn n <= 0: nichts überspringen
    dropChars n (_:xs) = dropChars (n - 1) xs -- Überspringe ein Zeichen, rekursiv


-- ===============================================================
-- Kehrt eine Liste rekursiv um (ineffizient)
-- ===============================================================
reverse1 :: [a] -> [a]
reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]  -- rekursiver Aufruf mit ersten Element am Ende (++ ist teuer: O(n) bei jedem Aufruf)

-- Kehrt eine Liste effizient um mithilfe eines Akkumulators
reverse2 :: [a] -> [a]
reverse2 xs = rev xs []
  where
    rev :: [a] -> [a] -> [a]
    rev [] acc     = acc              -- leerer String = Akkumulator enthält Ergebnis
    rev (x:xs) acc = rev xs (x : acc) -- erstes Element an den Anfang des Akkumulators anhängen und rekursiv mit dem Rest aufrufen

-- ===============================================================
-- Gibt alle Anfangssegmente (Präfixe) einer Liste zurück
-- Beispiel: inits [1,2] == [[], [1], [1,2]]
-- ===============================================================

inits :: [a] -> [[a]]
inits xs = initsH [] xs
  where
    -- Hilfsfunktion mit Akkumulator
    -- acc sammelt das aktuelle Anfangsstück (umgekehrt)
    initsH acc []     = [reverse acc]                   -- Letztes Element: ganzes xs
    initsH acc (y:ys) = reverse acc : initsH (y:acc) ys -- Fügt y am Anfang von acc hinzu und rekursiv weitermachen

-- ===============================================================
-- Gibt alle Endsegmente (Suffixe) einer Liste zurück
-- Beispiel: tails [1,2] == [[1,2], [2], []]
-- ===============================================================
tails :: [a] -> [[a]]
tails []     = [[]]                 -- leeres Suffix
tails (x:xs) = (x:xs) : tails xs    -- rekursiv auf xs anwenden

-- ===============================================================
-- Fügt ein Element an jeder möglichen Position in eine Liste ein
-- Beispiel: insert 1 [2,3] == [[1,2,3],[2,1,3],[2,3,1]]
-- ===============================================================
insert :: a -> [a] -> [[a]]
insert x []     = [[x]]  -- Nur eine Position in leerer Liste
insert x (y:ys) =
  (x:y:ys) : prepend y (insert x ys)  -- x vorne einsetzen, dann rekursiv
  where
    
    -- Hilfsfunktion fügt y wieder vorne an jede Teilliste an
    prepend _ []         = []
    prepend z (lst:lsts) = (z:lst) : prepend z lsts

-- ===============================================================
-- Berechnet alle Permutationen einer Liste
-- Beispiel: perms [1,2] == [[1,2], [2,1]]
-- ===============================================================
perms :: [a] -> [[a]]
perms []     = [[]]                 -- Eine Permutation: die leere Liste
perms (x:xs) = permsH (perms xs)    -- Rekursiv Permutationen ohne x berechnen
  where
    -- Hilfsfunktion f ügt x in alle Positionen jeder Permutation p ein
    permsH []     = []
    permsH (p:ps) = insert x p ++ permsH ps



-- ===============================================================
-- ===============================================================
-- Optionale Aufgabe 
-- ===============================================================
-- Gibt den Index des ersten Auftretens eines Int-Wertes zurück
-- Rückgabetyp Maybe Int: Just index oder Nothing
-- Beispiel: indexOf 3 [1,2,3] == Just 2
--           indexOf 5 [1,2,3] == Nothing
-- ===============================================================
indexOf :: Int -> [Int] -> Maybe Int
indexOf x = go 0
  where
    -- Hilfsfunktion 
    go _ [] = Nothing                     -- Element nicht gefunden
    go i (y:ys)
      | x == y    = Just i               -- Gefunden
      | otherwise = go (i + 1) ys        -- Weitersuchen mit erhöhtem Index








