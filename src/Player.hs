module Player
    ( Player(..)
    , playerName
    , playerType
    , playerHand
    , PlayerType(..)
    , createPlayer
    ) where

import Card (Card)

data PlayerType = Declarer | Defender
    deriving (Show, Eq)

data Player = Player
    { playerName :: String
    , playerType :: Maybe PlayerType
    , playerHand :: [Card]
    } deriving (Show)

createPlayer :: String -> Player
createPlayer name = Player
    { playerName = name
    , playerType = Nothing
    , playerHand = []
    }
