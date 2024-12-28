module Game
    ( GameState(..)
    , Phase(..)
    , GameVariant(..)
    , Trick(..)
    , PlayedCard(..)
    , GameResult(..)
    , initGame
    , makeBid
    , respondToBid
    , BidResponse(..)
    , pickUpSkat
    , declareSkat
    , playCard
    , getCurrentTrick
    , isValidPlay
    , getTrickWinner
    , calculateScore
    ) where

import Card (Card(..), Suit(..), Rank(..), deck)
import Player (Player(..), PlayerType(..), createPlayer)
import System.Random (StdGen, RandomGen, randomR)
import Data.List (find, (\\), maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust, isNothing)

data Phase = Bidding | Picking | Declaring | Playing | GameOver
    deriving (Show, Eq)

data GameVariant = 
      Grand    -- Jacks are the only trumps
    | Null     -- No tricks should be taken
    | Suit Suit -- Normal game with a trump suit
    deriving (Show, Eq)

data BidResponse = Accept | Pass
    deriving (Show, Eq)

data PlayedCard = PlayedCard
    { playedBy :: Int  -- Index of player who played the card
    , playedCard :: Card     -- The card that was played
    } deriving (Show, Eq)

data Trick = Trick
    { leadPlayer :: Int        -- Index of player who led the trick
    , cardsPlayed :: [PlayedCard]  -- Cards played in this trick
    } deriving (Show, Eq)

-- Game result with scoring information
data GameResult = GameResult
    { gamePoints :: Int        -- Base points for the game
    , multiplier :: Int        -- Game level multiplier
    , totalScore :: Int        -- Final score after multiplier
    , schneider :: Bool        -- Whether Schneider was achieved
    , schwarz :: Bool         -- Whether Schwarz was achieved
    , won :: Bool             -- Whether the declarer won
    } deriving (Show)

data GameState = GameState
    { players :: [Player]
    , skat :: [Card]
    , currentPhase :: Phase
    , gameDeck :: [Card]
    , rng :: StdGen
    , currentBid :: Maybe Int
    , currentBidder :: Maybe Int  -- Index of current bidding player
    , currentResponder :: Maybe Int  -- Index of player responding to bid
    , passedPlayers :: [Int]  -- Indices of players who have passed
    , declaredVariant :: Maybe GameVariant  -- Declared game variant
    , originalSkat :: [Card]  -- Keep track of original Skat for scoring
    , tricks :: [Trick]       -- Completed tricks
    , currentTrick :: Maybe Trick  -- Current trick in progress
    , trickWinner :: Maybe Int    -- Winner of the last trick (leads next)
    } deriving (Show)

-- Card point values in Skat
cardPoints :: Card -> Int
cardPoints (Card _ rank) = case rank of
    Ace -> 11
    Ten -> 10
    King -> 4
    Queen -> 3
    Jack -> 2
    _ -> 0

-- Get all cards played by a player in completed tricks
playerCards :: GameState -> Int -> [Card]
playerCards game playerIdx =
    concatMap (\trick -> 
        map playedCard $ filter (\pc -> playedBy pc == playerIdx) (cardsPlayed trick)
    ) (tricks game)

-- Calculate points in a list of cards
calculateCardPoints :: [Card] -> Int
calculateCardPoints = sum . map cardPoints

-- Base game values
baseGameValue :: GameVariant -> Int
baseGameValue Grand = 24
baseGameValue Null = 23
baseGameValue (Suit suit) = case suit of
    Clubs -> 12
    Spades -> 11
    Hearts -> 10
    Diamonds -> 9

-- Count jacks in hand
countJacks :: [Card] -> Int
countJacks = length . filter isJack

-- Calculate game level (multiplier)
calculateMultiplier :: GameVariant -> [Card] -> [Card] -> Int
calculateMultiplier Null _ _ = 1  -- Null games have no multiplier
calculateMultiplier variant hand skat =
    let allCards = hand ++ skat
        withJacks = countJacks allCards + 1  -- With or without jacks
        trumps = case variant of
            Grand -> withJacks
            Suit trumpSuit -> 
                withJacks +  -- With/without jacks
                (if any (\(Card s r) -> s == trumpSuit && r /= Jack) allCards then 1 else 0)  -- With/without trump
    in trumps

-- Calculate if Schneider was achieved (winner got 90+ points)
isSchneider :: GameState -> Int -> Bool
isSchneider game winnerIdx =
    let points = calculateCardPoints $ playerCards game winnerIdx
    in points >= 90

-- Calculate if Schwarz was achieved (winner got all tricks)
isSchwarz :: GameState -> Int -> Bool
isSchwarz game winnerIdx =
    all (\trick -> 
        let winner = getTrickWinner (fromJust $ declaredVariant game) trick
        in winner == winnerIdx
    ) (tricks game)

-- Calculate if the declarer won
didDeclarerWin :: GameState -> Int -> Bool
didDeclarerWin game declarerIdx =
    let variant = fromJust $ declaredVariant game
        declarerPoints = calculateCardPoints $ playerCards game declarerIdx
    in case variant of
        Null -> not $ any (\trick -> getTrickWinner variant trick == declarerIdx) (tricks game)
        _ -> declarerPoints > 60

-- Calculate final score for the game
calculateScore :: GameState -> GameResult
calculateScore game =
    let declarerIdx = fromJust $ currentBidder game
        variant = fromJust $ declaredVariant game
        declarer = players game !! declarerIdx
        originalHand = playerHand declarer
        multiplierValue = calculateMultiplier variant originalHand (originalSkat game)
        baseValue = baseGameValue variant
        schneiderAchieved = isSchneider game declarerIdx
        schwarzAchieved = isSchwarz game declarerIdx
        wonGame = didDeclarerWin game declarerIdx
        
        -- Additional multipliers
        schneiderMultiplier = if schneiderAchieved then 1 else 0
        schwarzMultiplier = if schwarzAchieved then 1 else 0
        
        -- Total multiplier including base game level
        totalMultiplier = multiplierValue + schneiderMultiplier + schwarzMultiplier
        
        -- Final score
        finalScore = if wonGame 
                    then baseValue * totalMultiplier
                    else -(baseValue * totalMultiplier)
    in GameResult
        { gamePoints = baseValue
        , multiplier = totalMultiplier
        , totalScore = finalScore
        , schneider = schneiderAchieved
        , schwarz = schwarzAchieved
        , won = wonGame
        }

-- Standard bids in Skat
validBids :: [Int]
validBids = [18, 20, 22, 23, 24, 27, 30, 33, 35, 36, 40, 44, 45, 46, 48, 50, 54, 55, 59, 60]

-- Fisher-Yates shuffle implementation
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle g [] = ([], g)
shuffle g [x] = ([x], g)
shuffle g xs = 
    let (i, g') = randomR (0, length xs - 1) g
        x = xs !! i
        rest = take i xs ++ drop (i + 1) xs
        (shuffled, g'') = shuffle g' rest
    in (x:shuffled, g'')

-- Deal cards to players and skat
dealCards :: GameState -> GameState
dealCards game =
    let (shuffledDeck, newRng) = shuffle (rng game) (gameDeck game)
        (hand1, rest1) = splitAt 10 shuffledDeck
        (hand2, rest2) = splitAt 10 rest1
        (hand3, skatCards) = splitAt 10 rest2
        updatedPlayers = zipWith (\p h -> p { playerHand = h }) (players game) [hand1, hand2, hand3]
    in game
        { players = updatedPlayers
        , skat = skatCards
        , originalSkat = skatCards
        , rng = newRng
        , currentPhase = Bidding
        , currentBid = Just 18  -- Start with minimum bid
        , currentBidder = Just 0  -- First player starts bidding
        , currentResponder = Just 1  -- Second player responds
        , passedPlayers = []
        , tricks = []
        , currentTrick = Nothing
        , trickWinner = Nothing
        }

-- Make a bid
makeBid :: GameState -> Maybe Int -> Either String GameState
makeBid _ Nothing = Left "No bid provided"
makeBid game (Just bid)
    | currentPhase game /= Bidding = Left "Not in bidding phase"
    | currentBidder game == Nothing = Left "No current bidder"
    | Just bid <= currentBid game = Left "Bid must be higher than current bid"
    | not (bid `elem` validBids) = Left "Invalid bid value"
    | otherwise = Right $ game { currentBid = Just bid }

-- Respond to a bid
respondToBid :: GameState -> BidResponse -> Either String GameState
respondToBid game response
    | currentPhase game /= Bidding = Left "Not in bidding phase"
    | currentResponder game == Nothing = Left "No current responder"
    | otherwise = case response of
        Accept -> handleAccept game
        Pass -> handlePass game

-- Handle an accept response
handleAccept :: GameState -> Either String GameState
handleAccept game =
    let nextResp = nextResponder game
    in Right $ case nextResp of
        Nothing -> finalizeBidding game { currentBidder = currentResponder game }
        Just resp -> game 
            { currentResponder = Just resp
            , currentBidder = currentResponder game
            }

-- Handle a pass response
handlePass :: GameState -> Either String GameState
handlePass game = 
    let responderIdx = currentResponder game
        newPassedPlayers = maybe (passedPlayers game) (:passedPlayers game) responderIdx
    in if length newPassedPlayers >= 2
        then Right $ finalizeBidding game { passedPlayers = newPassedPlayers }
        else Right $ game 
            { passedPlayers = newPassedPlayers
            , currentResponder = nextResponder game
            }

-- Get the next responder, skipping passed players
nextResponder :: GameState -> Maybe Int
nextResponder game = 
    let current = currentResponder game
        next i = (i + 1) `mod` 3
        isValidResponder i = i `notElem` passedPlayers game && Just i /= currentBidder game
    in case current of
        Nothing -> Nothing
        Just i -> find isValidResponder [next i, next (next i)]

-- Finalize the bidding phase
finalizeBidding :: GameState -> GameState
finalizeBidding game = 
    let winner = currentBidder game
        updatedPlayers = case winner of
            Just idx -> zipWith (\i p -> p { playerType = if i == idx 
                                                         then Just Declarer 
                                                         else Just Defender })
                               [0..] (players game)
            Nothing -> players game
    in game 
        { currentPhase = Picking
        , players = updatedPlayers
        }

-- Pick up Skat and discard two cards
pickUpSkat :: GameState -> [Int] -> Either String GameState
pickUpSkat game cardIndices
    | currentPhase game /= Picking = Left "Not in picking phase"
    | length cardIndices /= 2 = Left "Must discard exactly two cards"
    | any (< 0) cardIndices = Left "Invalid card index"
    | any (>= 12) cardIndices = Left "Invalid card index"
    | otherwise = case currentBidder game of
        Nothing -> Left "No declarer found"
        Just declarerIdx -> 
            let declarer = players game !! declarerIdx
                oldHand = playerHand declarer
                newHand = (oldHand ++ skat game) \\ [oldHand !! i | i <- cardIndices]
                discardedCards = [oldHand !! i | i <- cardIndices]
                updatedPlayers = zipWith (\i p -> if i == declarerIdx 
                                                then p { playerHand = newHand }
                                                else p) 
                                       [0..] (players game)
            in Right $ game 
                { players = updatedPlayers
                , skat = discardedCards
                , currentPhase = Declaring
                }

-- Declare game variant
declareSkat :: GameState -> GameVariant -> Either String GameState
declareSkat game variant
    | currentPhase game /= Declaring = Left "Not in declaring phase"
    | otherwise = Right $ game 
        { declaredVariant = Just variant
        , currentPhase = Playing
        , trickWinner = currentBidder game  -- Declarer leads first trick
        }

-- Check if a card is a jack
isJack :: Card -> Bool
isJack (Card _ Jack) = True
isJack _ = False

-- Get the suit of a card (accounting for jacks being special in Grand)
effectiveSuit :: GameVariant -> Card -> Suit
effectiveSuit Grand card | isJack card = Clubs  -- All jacks are treated as clubs in Grand
effectiveSuit _ (Card suit _) = suit

-- Check if a card is trump
isTrump :: GameVariant -> Card -> Bool
isTrump Grand card = isJack card
isTrump Null _ = False
isTrump (Suit trumpSuit) card@(Card suit rank)
    | isJack card = True
    | suit == trumpSuit = True
    | otherwise = False

-- Get card value for trick comparison
cardStrength :: GameVariant -> Card -> Int
cardStrength variant card@(Card suit rank)
    | isJack card = 
        case rank of
            Jack -> case suit of
                Clubs -> 100
                Spades -> 99
                Hearts -> 98
                Diamonds -> 97
            _ -> 0
    | otherwise = case rank of
        Ace -> 11
        Ten -> 10
        King -> 4
        Queen -> 3
        Nine -> 2
        Eight -> 1
        Seven -> 0
        _ -> 0  -- Should never happen

-- Compare two cards in the context of a trick
compareCards :: GameVariant -> Suit -> Card -> Card -> Ordering
compareCards variant leadSuit card1 card2
    | isTrump variant card1 && isTrump variant card2 =
        comparing (cardStrength variant) card1 card2
    | isTrump variant card1 = GT
    | isTrump variant card2 = LT
    | effectiveSuit variant card1 == leadSuit && effectiveSuit variant card2 == leadSuit =
        comparing (cardStrength variant) card1 card2
    | effectiveSuit variant card1 == leadSuit = GT
    | effectiveSuit variant card2 == leadSuit = LT
    | otherwise = EQ

-- Check if a player can follow suit
canFollowSuit :: GameVariant -> Suit -> [Card] -> Bool
canFollowSuit variant leadSuit hand =
    any (\card -> effectiveSuit variant card == leadSuit) hand

-- Check if a play is valid
isValidPlay :: GameState -> Int -> Card -> Bool
isValidPlay game playerIdx card
    | currentPhase game /= Playing = False
    | not (card `elem` playerHand (players game !! playerIdx)) = False
    | isNothing (currentTrick game) && isNothing (trickWinner game) = False
    | otherwise = case currentTrick game of
        Nothing -> True  -- First card of new trick
        Just trick -> 
            let firstPlayed = head $ cardsPlayed trick
                leadCard = playedCard firstPlayed
                leadSuit = effectiveSuit (fromJust $ declaredVariant game) leadCard
                hand = playerHand $ players game !! playerIdx
            in if canFollowSuit (fromJust $ declaredVariant game) leadSuit hand
               then effectiveSuit (fromJust $ declaredVariant game) card == leadSuit
               else True

-- Play a card
playCard :: GameState -> Int -> Card -> Either String GameState
playCard game playerIdx card
    | currentPhase game /= Playing = Left "Not in playing phase"
    | not (isValidPlay game playerIdx card) = Left "Invalid play"
    | otherwise = Right $ case currentTrick game of
        Nothing -> 
            -- Start new trick
            game { currentTrick = Just $ Trick playerIdx [PlayedCard playerIdx card]
                , players = updatePlayerHand game playerIdx card
                }
        Just trick ->
            let newTrick = trick { cardsPlayed = cardsPlayed trick ++ [PlayedCard playerIdx card] }
                updatedGame = game { currentTrick = Just newTrick
                                 , players = updatePlayerHand game playerIdx card
                                 }
            in if length (cardsPlayed newTrick) == 3
               then finalizeTrick updatedGame
               else updatedGame

-- Update a player's hand after playing a card
updatePlayerHand :: GameState -> Int -> Card -> [Player]
updatePlayerHand game playerIdx card =
    zipWith (\i p -> if i == playerIdx
                     then p { playerHand = filter (/= card) (playerHand p) }
                     else p)
            [0..] (players game)

-- Get the winner of a trick
getTrickWinner :: GameVariant -> Trick -> Int
getTrickWinner variant trick =
    let cards = cardsPlayed trick
        firstPlayed = head cards
        leadSuit = effectiveSuit variant (playedCard firstPlayed)
        winningCard = maximumBy (\c1 c2 -> compareCards variant leadSuit (playedCard c1) (playedCard c2)) cards
    in playedBy winningCard

-- Finalize a trick
finalizeTrick :: GameState -> GameState
finalizeTrick game =
    let trick = fromJust $ currentTrick game
        variant = fromJust $ declaredVariant game
        winner = getTrickWinner variant trick
    in game { currentTrick = Nothing
           , tricks = tricks game ++ [trick]
           , trickWinner = Just winner
           , currentPhase = if all (null . playerHand) (players game) 
                           then GameOver 
                           else Playing
           }

-- Get the current trick
getCurrentTrick :: GameState -> Maybe Trick
getCurrentTrick = currentTrick

-- Initialize game state
initGame :: StdGen -> [String] -> GameState
initGame gen playerNames =
    dealCards $ GameState
    { players = map createPlayer playerNames
    , skat = []
    , originalSkat = []
    , currentPhase = Bidding
    , gameDeck = deck
    , rng = gen
    , currentBid = Nothing
    , currentBidder = Nothing
    , currentResponder = Nothing
    , passedPlayers = []
    , declaredVariant = Nothing
    , tricks = []
    , currentTrick = Nothing
    , trickWinner = Nothing
    }
