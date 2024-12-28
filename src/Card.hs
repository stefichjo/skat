module Card
    ( Card(..)
    , Suit(..)
    , Rank(..)
    , deck
    , cardValue
    ) where

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Eq, Ord, Enum)

instance Show Suit where
    show Clubs = "♣"
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"

data Rank = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum)

instance Show Rank where
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

data Card = Card { suit :: Suit, rank :: Rank }
    deriving (Eq)

instance Show Card where
    show (Card s r) = show r ++ show s

-- Standard Skat deck of 32 cards
deck :: [Card]
deck = [Card s r | s <- [Clubs .. Diamonds], r <- [Seven .. Ace]]

-- Card value in points
cardValue :: Card -> Int
cardValue (Card _ r) = case r of
    Ten  -> 10
    Jack -> 2
    Queen -> 3
    King -> 4
    Ace -> 11
    _ -> 0
