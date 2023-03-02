module Cards where

data CardValue
  = PipCard Int -- From 1 to 10
  | Jack
  | Queen
  | King
      deriving (Show, Eq, Ord)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
      deriving (Show, Eq, Ord)

data AcesHigh = AcesHigh
data AcesLow  = AcesLow

data Card (a :: *) =
  Card CardValue Suit
    deriving (Eq)

instance Show (Card a) where
  show (Card value suit) = showValue value ++ showSuit suit
    where
      showValue (PipCard 1)  = "A"
      showValue (PipCard n)  = show n
      showValue Jack         = "J" 
      showValue Queen        = "Q" 
      showValue King         = "K" 

      showSuit Clubs         = "♣"
      showSuit Diamonds      = "♦"
      showSuit Hearts        = "♥" -- "\x1B[31m♥\x1B[0m" -- red in terminal
      showSuit Spades        = "♠" -- "\x1B[31m♠\x1B[0m"

   {- Uncomment if you don't have Unicode:

      showSuit Clubs         = "C"
      showSuit Diamonds      = "D"
      showSuit Hearts        = "H"
      showSuit Spades        = "S"
   -}

--------------------------------------------------------------------------------
-- TODO: Uncomment and implement the following two instances

instance Ord (Card AcesHigh) where
  (<=) :: Card AcesHigh -> Card AcesHigh -> Bool
  Card val1 suit1 <= Card val2 suit2 =
    case (val1, val2) of
      (PipCard 1, PipCard 1)  -> suit1 <= suit2
      (PipCard 1, _)          -> False
      (_, PipCard 1)          -> True
      (_, _)                  -> val1 < val2 || (val1 == val2 && suit1 <= suit2)

instance Ord (Card AcesLow) where
  (<=) :: Card AcesLow -> Card AcesLow -> Bool
  Card val1 suit1 <= Card val2 suit2 = 
    val1 < val2 || (val1 == val2 && suit1 <= suit2)


--------------------------------------------------------------------------------

fullDeckAcesHigh :: [Card AcesHigh]
fullDeckAcesLow  :: [Card AcesLow]

fullDeckAcesHigh = fullDeck True
fullDeckAcesLow  = fullDeck False

-- All 52 cards in ascending order, treating aces high (True) or low (False).
fullDeck :: Bool -> [Card a]
fullDeck False = 
  let
    vals = map PipCard [1..10] ++ [Jack, Queen, King]
    suits = [Clubs, Diamonds, Hearts, Spades]
  in  
    [Card v s | v <- vals, s <- suits]
fullDeck True = 
  let
    vals = map PipCard [2..10] ++ [Jack, Queen, King, PipCard 1]
    suits = [Clubs, Diamonds, Hearts, Spades]
  in  
    [Card v s | v <- vals, s <- suits]