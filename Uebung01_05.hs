module Uebung01_05 where 

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Enum, Eq)

data Rank = Two | Three | Four | Five | Six | Seven 
                | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Enum, Eq)

data Card = Card Rank Suit
    deriving(Eq)
    

instance Show Card where
  show (Card value rank) = show value ++ " of " ++ show rank

getCardValue :: Card -> Int
getCardValue (Card rank _) = case rank of
    Ace     -> 11
    _       -> min 10 (fromEnum rank + 2)
    
newtype Hand = Hand [Card]
  deriving (Show, Eq)

(<+>) :: Hand -> Hand -> Hand
(<+>) (Hand cards1) (Hand cards2) = Hand (cards1 ++ cards2)  

fullDeck :: Hand
fullDeck = Hand [Card rank suit |rank <- [Two .. Ace], suit <- [Hearts .. Spades]]

numOfAces :: Hand -> Int
numOfAces (Hand cards) = length [rank | Card rank _ <- cards, rank == Ace]

getValue :: Hand -> Int
getValue (Hand cards) =
  let total = sum [getCardValue card | card <- cards]
      aces  = numOfAces (Hand cards)
  in if total <= 21
     then total
     else minmax total aces

minmax :: Int -> Int -> Int
minmax val ace |  val<=21 || ace == 0 = val
minmax val ace                        = minmax (val - 10) (ace - 1)


-- test
{-
c1 = Card Ace Hearts
c2 = Card Ace Spades
c3 = Card Two Diamonds
hand = Hand [c1,c2,c3]
λ: 14
-}
