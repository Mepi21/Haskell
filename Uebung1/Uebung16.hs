-- ============================================
-- Ein binärer Suchbaum mit Int-Werten:
-- ============================================
data SearchTree = Empty                             -- leerer Baum
                | Node Int SearchTree SearchTree    -- Knoten mit Int-Wert, linkem und rechtem Teilbaum
  deriving (Show, Eq)

-- ============================================
-- Fügt ein Element in den Suchbaum ein, sodass die Eigenschaft des Suchbaums erhalten bleibt
-- ============================================
insert :: Int -> SearchTree -> SearchTree
insert x Empty = Node x Empty Empty                   -- leeres Blatt -> neuer Knoten
insert x (Node n left right)
  | x == n = Node n left right                        -- kein Einfügen von Duplikaten
  | x < n  = Node n (insert x left) right             -- links einfügen, wenn kleiner
  | x > n  = Node n left (insert x right)             -- rechts einfügen, wenn größer

-- ============================================
-- Prüft, ob ein Element im Suchbaum enthalten ist
-- ============================================
isElem :: Int -> SearchTree -> Bool
isElem _ Empty = False                                -- Wenn der Baum leer ist, kann das gesuchte Element nicht enthalten sein            
isElem x (Node n left right)
  | x == n = True                                     -- Wenn der aktuelle Knoten den gesuchten Wert enthält, ist das Element gefunden 
  | x < n  = isElem x left                            -- Wenn der gesuchte Wert kleiner ist, suche im linken Teilbaum weiter
  | x > n  = isElem x right                           -- Wenn der gesuchte Wert größer ist, suche im rechten Teilbaum weiter

-- ============================================
-- Löscht ein Element aus dem Baum, falls es existiert
-- ============================================
delete :: Int -> SearchTree -> SearchTree
delete _ Empty = Empty                              -- Wenn der Baum leer ist, gibt es nichts zu löschen
delete x (Node n left right)
  | x < n  = Node n (delete x left) right           -- Wenn das zu löschende Element kleiner ist, im linken Teilbaum löschen
  | x > n  = Node n left (delete x right)           -- Wenn es größer ist, im rechten Teilbaum löschen
  | x == n = deleteNode (Node n left right)         -- Wenn das Element gefunden wurde, deleteNode aufrufen

-- ============================================
-- Hilfsfunktion: behandelt den Fall, wenn ein bestimmter Knoten (mit Wert n) gelöscht werden soll
-- ============================================
deleteNode :: SearchTree -> SearchTree
deleteNode (Node _ Empty right) = right             -- Wenn der linke Teilbaum leer ist, ersetzt der rechte Teilbaum den Knoten
deleteNode (Node _ left Empty)  = left              -- Wenn der rechte Teilbaum leer ist, ersetzt der linke Teilbaum den Knoten
deleteNode (Node _ left right)  =
  let minRight = findMin right                      -- Wenn beide Teilbäume existieren, suche das kleinste Element im rechten Teilbaum
  in Node minRight left (delete minRight right)     -- Ersetze den Knoten mit diesem Minimum und lösche es dann rekursiv aus dem rechten Teilbaum

-- ============================================
-- Hilfsfunktion: findet das kleinste Element im (Teil-)Baum
-- ============================================
findMin :: SearchTree -> Int
findMin (Node n Empty _) = n                        -- Wenn der linke Teilbaum leer ist, ist dies das kleinste Element
findMin (Node _ left _)  = findMin left             -- Andernfalls suche weiter im linken Teilbaum

