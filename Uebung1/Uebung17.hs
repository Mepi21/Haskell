-- ============================================
-- Polymorpher algebraischer Datentyp für Bäume
-- ============================================
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)
-- Ein Tree besteht entweder aus:
--  - einem Blatt mit einem Wert (Leaf a)
--  - einem Knoten, der zwei Teilbäume enthält (Node left right)
-- Der Typ ist polymorph: Tree kann z. B. Tree Int, Tree String usw. sein

-- ============================================
-- Rekursive Funktion: summiert die Werte aller Blätter
-- ============================================
sumTree1 :: Tree Int -> Int
sumTree1 (Leaf i) = i                           -- Ein Blatt: gib den Wert zurück
sumTree1 (Node t1 t2) = sumTree1 t1 + sumTree1 t2
-- Ein Knoten: summiere die Ergebnisse der Teilbäume rekursiv

-- ============================================
-- Version mit Akkumulator (Tail Recursion Style)
-- ============================================
sumTree2 :: Tree Int -> Int
sumTree2 tree = sumTree2' tree 0               -- Akkumulation beginnt bei 0
  where
    sumTree2' :: Tree Int -> Int -> Int
    sumTree2' (Leaf i) acc     = acc + i       -- Blatt: addiere zum Akkumulator
    sumTree2' (Node t1 t2) acc = sumTree2' t1 (sumTree2' t2 acc)
    -- Wichtig: zuerst rechter Teilbaum in den Akkumulator, dann linker
    -- Reihenfolge wirkt „verkehrt“, ist aber korrekt wegen Akkumulatorfluss

-- ============================================
-- Spiegelt den Baum entlang der vertikalen Achse
-- ============================================
mirrorTree :: Tree a -> Tree a
mirrorTree (Leaf i) = Leaf i                                   -- Blatt bleibt unverändert
mirrorTree (Node t1 t2) = Node (mirrorTree t2) (mirrorTree t1)
-- Vertauscht die linken und rechten Teilbäume rekursiv

-- ============================================
-- Extrahiert alle Blätter in eine Liste (von links nach rechts)
-- ============================================
toList :: Tree a -> [a]
toList (Leaf i) = [i]                          -- Blatt: ergibt Liste mit einem Element
toList (Node t1 t2) = toList t1 ++ toList t2
-- Knoten: konkateniere die Blätterlisten der beiden Teilbäume

-- ============================================
-- Beispielbaum:
--       Node
--      /    \
--   Leaf 1  Node
--           /   \
--       Leaf 2  Leaf 3
-- 
-- Entspricht: (1) + ((2) + (3)) = 6
-- Struktur: Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
-- ============================================
exampleTree :: Tree Int
exampleTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
