-- ============================================
-- Definition der Ränge (Rank) einer Karte
-- ============================================
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded)
-- Show   → Ermöglicht das Anzeigen als String
-- Eq     → Vergleichbarkeit (z. B. für Tests)
-- Enum   → Ermöglicht Aufzählungen (z. B. [Two .. Ace])
-- Bounded → Gibt minBound = Two, maxBound = Ace

-- ============================================
-- Definition der Farben (Suit) einer Karte
-- ============================================
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Eq, Enum, Bounded)
-- Gleiches Prinzip wie bei Rank – Farben/Symbole der Karten

-- ============================================
-- Eine Karte besteht aus einem Rang und einer Farbe
-- ============================================
data Card = Card Rank Suit
  deriving (Show, Eq)
-- Card kombiniert Rang und Farbe zu einer Spielkarte
-- z. B. Card Ace Spades = Pik Ass

-- ============================================
-- Bestimme den Wert einer Karte beim Black Jack
-- ============================================
getCardValue :: Card -> Int
getCardValue (Card rank _) =  -- Farbe ist irrelevant für den Wert, daher _
  case rank of
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    Eight -> 8
    Nine  -> 9
    Ten   -> 10
    Jack  -> 10
    Queen -> 10
    King  -> 10
    Ace   -> 11  -- Ass zählt erstmal 11 Punkte

-- ============================================
-- Eine Hand ist entweder leer oder eine Karte mit Rest der Hand
-- ============================================
data Hand = Empty | Add Card Hand
  deriving (Show, Eq)
-- Repräsentiert eine Hand als verkettete Struktur:
-- Add c1 (Add c2 (... Empty)) = [c1, c2, ...]

-- ============================================
-- Kombiniert zwei Hände zu einer
-- ============================================
(<+>) :: Hand -> Hand -> Hand
Empty     <+> h = h                       -- leere Hand + h = h
(Add c h) <+> h2 = Add c (h <+> h2)       -- fügt c vorne an und verkettet rekursiv

-- ============================================
-- Vollständiges Kartendeck erzeugen (52 Karten)
-- ============================================
fullDeck :: Hand
fullDeck = foldr Add Empty [Card rank suit | rank <- allRanks, suit <- allSuits]
  where
    allRanks = [minBound .. maxBound] :: [Rank]  -- Alle Ränge durch Enum + Bounded
    allSuits = [minBound .. maxBound] :: [Suit]  -- Alle Farben

-- foldr Add Empty [...] → baut Hand durch Add-Konstruktor auf

-- ============================================
-- Zählt die Anzahl der Asse in einer Hand
-- ============================================
numOfAces :: Hand -> Int
numOfAces Empty = 0                                      -- keine Karten = keine Asse
numOfAces (Add (Card Ace _) rest) = 1 + numOfAces rest   -- Ass gefunden → +1
numOfAces (Add _ rest) = numOfAces rest                  -- sonst weiterzählen

-- ============================================
-- Berechnet den Gesamtwert einer Hand
-- Berücksichtigt, dass Asse 1 oder 11 wert sein können
-- ============================================
getValue :: Hand -> Int
getValue hand = adjustForAces total aceCount
  where
    total = getRawValue hand      -- Summe aller Kartenwerte (Asse = 11)
    aceCount = numOfAces hand     -- Wie viele Asse gibt es?

    -- Rekursiv Wert berechnen als Summe aller Kartenwerte
    getRawValue :: Hand -> Int
    getRawValue Empty = 0
    getRawValue (Add c rest) = getCardValue c + getRawValue rest

    -- Reduziert den Gesamtwert, indem Asse von 11 auf 1 gesetzt werden,
    -- falls der Wert über 21 liegt (Blackjack-Regel)
    adjustForAces :: Int -> Int -> Int
    adjustForAces val 0 = val                       -- keine Asse mehr zum Anpassen
    adjustForAces val aces
      | val <= 21 = val                             -- Gesamtwert passt → fertig
      | otherwise = adjustForAces (val - 10) (aces - 1)
        -- Ass zählt nur noch 1 → -10 Punkte, da vorher 11
