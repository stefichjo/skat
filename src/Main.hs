module Main where

import System.Random (newStdGen)
import System.IO (hFlush, stdout)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Control.Monad (when)
import Game
    ( GameState(..)
    , Phase(..)
    , GameVariant(..)
    , BidResponse(..)
    , Trick(..)
    , PlayedCard(..)
    , initGame
    , makeBid
    , respondToBid
    , pickUpSkat
    , declareSkat
    , playCard
    , getCurrentTrick
    , isValidPlay
    , getTrickWinner
    , calculateScore
    , won
    , gamePoints
    , multiplier
    , schneider
    , schwarz
    , totalScore
    , playedBy
    , playedCard
    , cardsPlayed
    , leadPlayer
    )
import Card (Card(..), Suit(..))
import Player (Player(..), playerHand)

showPlayerHand :: Int -> GameState -> String
showPlayerHand playerIndex game =
    let player = players game !! playerIndex
        handStr = unwords (map show (playerHand player))
    in "Player " ++ show (playerIndex + 1) ++ "'s hand: " ++ handStr

showGameState :: GameState -> IO ()
showGameState game = do
    putStrLn $ "\nCurrent phase: " ++ show (currentPhase game)
    putStrLn $ "Current bid: " ++ maybe "No bid yet" show (currentBid game)
    putStrLn $ "Current bidder: Player " ++ maybe "None" (show . (+1)) (currentBidder game)
    putStrLn $ "Current responder: Player " ++ maybe "None" (show . (+1)) (currentResponder game)
    putStrLn $ "Passed players: Players " ++ 
               if null (passedPlayers game) 
                   then "None" 
                   else unwords (map (show . (+1)) (passedPlayers game))
    putStrLn $ "Declared variant: " ++ maybe "None" show (declaredVariant game)
    
    -- Show current trick if any
    case getCurrentTrick game of
        Just trick -> do
            putStrLn "\nCurrent trick:"
            mapM_ (\pc -> putStrLn $ "Player " ++ show (playedBy pc + 1) ++ ": " ++ show (playedCard pc))
                  (cardsPlayed trick)
        Nothing -> 
            putStrLn $ "\nTrick winner: Player " ++ maybe "None" (show . (+1)) (trickWinner game)
    
    putStrLn "\nCompleted tricks:"
    mapM_ (\trick -> do
            putStrLn $ "Led by Player " ++ show (leadPlayer trick + 1) ++ ":"
            mapM_ (\pc -> putStrLn $ "  Player " ++ show (playedBy pc + 1) ++ ": " ++ show (playedCard pc))
                  (cardsPlayed trick))
          (tricks game)
    
    putStrLn "\nHands:"
    mapM_ (\i -> putStrLn $ showPlayerHand i game) [0..2]
    putStrLn $ "Skat: " ++ unwords (map show (skat game))

    -- Show game result if game is over
    when (currentPhase game == GameOver) $ do
        let result = calculateScore game
            declarerIdx = fromJust $ currentBidder game
        putStrLn "\nGame Over!"
        putStrLn $ "Declarer (Player " ++ show (declarerIdx + 1) ++ ") " ++ 
                   if won result then "won!" else "lost!"
        putStrLn $ "Base game value: " ++ show (gamePoints result)
        putStrLn $ "Game level (multiplier): " ++ show (multiplier result)
        when (schneider result) $ putStrLn "Achieved Schneider! (+1 multiplier)"
        when (schwarz result) $ putStrLn "Achieved Schwarz! (+1 multiplier)"
        putStrLn $ "Final score: " ++ show (totalScore result)

getPlayerInput :: String -> IO String
getPlayerInput prompt = do
    putStr $ "\n" ++ prompt
    hFlush stdout
    getLine

handleBidding :: GameState -> IO GameState
handleBidding game
    | currentPhase game /= Bidding = return game
    | otherwise = do
        showGameState game
        case (currentBidder game, currentResponder game) of
            (Just bidderIdx, Just responderIdx) -> do
                putStrLn $ "\nPlayer " ++ show (responderIdx + 1) ++ "'s turn to respond"
                putStrLn $ "Current bid from Player " ++ show (bidderIdx + 1) ++ ": " ++ 
                          maybe "No bid yet" show (currentBid game)
                response <- getPlayerInput "Enter 'accept' or 'pass': "
                case response of
                    "accept" -> case respondToBid game Accept of
                        Left err -> do
                            putStrLn $ "\nError: " ++ err
                            handleBidding game
                        Right newGame -> do
                            putStrLn "\nBid accepted!"
                            if currentPhase newGame == Picking
                                then return newGame
                                else do
                                    putStrLn $ "\nPlayer " ++ show (bidderIdx + 1) ++ "'s turn to bid"
                                    bidStr <- getPlayerInput "Enter bid value: "
                                    handleBidResponse newGame bidStr
                    "pass" -> case respondToBid game Pass of
                        Left err -> do
                            putStrLn $ "\nError: " ++ err
                            handleBidding game
                        Right newGame -> do
                            putStrLn "\nPassed!"
                            if currentPhase newGame == Picking
                                then return newGame
                                else handleBidding newGame
                    _ -> do
                        putStrLn "\nInvalid input. Please enter 'accept' or 'pass'"
                        handleBidding game
            _ -> return game

handleBidResponse :: GameState -> String -> IO GameState
handleBidResponse game bidStr = 
    case (reads bidStr :: [(Int, String)]) of
        [(bid, "")] -> case makeBid game (Just bid) of
            Left err -> do
                putStrLn $ "\nError: " ++ err
                handleBidding game
            Right newGame -> handleBidding newGame
        _ -> do
            putStrLn "\nInvalid input. Please enter a number"
            handleBidding game

handlePicking :: GameState -> IO GameState
handlePicking game
    | currentPhase game /= Picking = return game
    | otherwise = do
        showGameState game
        case currentBidder game of
            Nothing -> return game
            Just declarerIdx -> do
                putStrLn $ "\nPlayer " ++ show (declarerIdx + 1) ++ ", you won the bidding!"
                putStrLn "The Skat has been added to your hand."
                putStrLn "Select two cards to discard (enter indices 0-11, separated by space):"
                let declarer = players game !! declarerIdx
                putStrLn $ "Your hand (with Skat): " ++ 
                          intercalate " " (zipWith (\i c -> show i ++ ":" ++ show c) 
                                                 [0..] (playerHand declarer ++ skat game))
                indices <- getPlayerInput "Enter two indices: "
                case reads ("[" ++ indices ++ "]") :: [([(Int)], String)] of
                    [(nums, "")] | length nums == 2 -> 
                        case pickUpSkat game nums of
                            Left err -> do
                                putStrLn $ "\nError: " ++ err
                                handlePicking game
                            Right newGame -> return newGame
                    _ -> do
                        putStrLn "\nInvalid input. Please enter two numbers separated by space"
                        handlePicking game

handleDeclaring :: GameState -> IO GameState
handleDeclaring game
    | currentPhase game /= Declaring = return game
    | otherwise = do
        showGameState game
        putStrLn "\nDeclare your game:"
        putStrLn "1. Grand (Jacks are the only trumps)"
        putStrLn "2. Null (No tricks should be taken)"
        putStrLn "3. Suit game (Choose a trump suit)"
        choice <- getPlayerInput "Enter your choice (1-3): "
        case choice of
            "1" -> handleVariant game Grand
            "2" -> handleVariant game Null
            "3" -> do
                putStrLn "\nChoose trump suit:"
                putStrLn "1. Clubs (♣)"
                putStrLn "2. Spades (♠)"
                putStrLn "3. Hearts (♥)"
                putStrLn "4. Diamonds (♦)"
                suit <- getPlayerInput "Enter suit (1-4): "
                case suit of
                    "1" -> handleVariant game (Suit Clubs)
                    "2" -> handleVariant game (Suit Spades)
                    "3" -> handleVariant game (Suit Hearts)
                    "4" -> handleVariant game (Suit Diamonds)
                    _ -> do
                        putStrLn "\nInvalid suit choice"
                        handleDeclaring game
            _ -> do
                putStrLn "\nInvalid choice"
                handleDeclaring game

handleVariant :: GameState -> GameVariant -> IO GameState
handleVariant game variant = 
    case declareSkat game variant of
        Left err -> do
            putStrLn $ "\nError: " ++ err
            handleDeclaring game
        Right newGame -> do
            putStrLn $ "\nDeclared game: " ++ show variant
            return newGame

handlePlaying :: GameState -> IO GameState
handlePlaying game
    | currentPhase game /= Playing = return game
    | otherwise = do
        showGameState game
        case trickWinner game of
            Nothing -> return game
            Just currentPlayer -> do
                putStrLn $ "\nPlayer " ++ show (currentPlayer + 1) ++ "'s turn"
                let hand = playerHand $ players game !! currentPlayer
                putStrLn "Your hand:"
                putStrLn $ intercalate " " (zipWith (\i c -> show i ++ ":" ++ show c) 
                                                   [0..] hand)
                cardIdx <- getPlayerInput "Enter card index to play: "
                case reads cardIdx :: [(Int, String)] of
                    [(idx, "")] | idx >= 0 && idx < length hand ->
                        let card = hand !! idx
                        in if isValidPlay game currentPlayer card
                           then case playCard game currentPlayer card of
                                Left err -> do
                                    putStrLn $ "\nError: " ++ err
                                    handlePlaying game
                                Right newGame -> 
                                    if currentPhase newGame == GameOver
                                    then return newGame
                                    else handlePlaying newGame
                           else do
                                putStrLn "\nInvalid play. Must follow suit if possible."
                                handlePlaying game
                    _ -> do
                        putStrLn "\nInvalid card index"
                        handlePlaying game

main :: IO ()
main = do
    gen <- newStdGen
    let playerNames = ["Player 1", "Player 2", "Player 3"]
    putStrLn "Welcome to Skat!"
    putStrLn "==============="
    putStrLn $ "\nPlayers: " ++ unwords playerNames
    
    let gameState = initGame gen playerNames
    
    putStrLn "\nStarting bidding phase..."
    biddingState <- handleBidding gameState
    
    putStrLn "\nStarting picking phase..."
    pickingState <- handlePicking biddingState
    
    putStrLn "\nStarting declaring phase..."
    declaringState <- handleDeclaring pickingState
    
    putStrLn "\nStarting playing phase..."
    finalState <- handlePlaying declaringState
    
    putStrLn "\nGame Over! Final state:"
    showGameState finalState
    
    case (currentBidder finalState, declaredVariant finalState) of
        (Just winner, Just variant) -> do
            putStrLn $ "\nPlayer " ++ show (winner + 1) ++ " was the declarer"
            putStrLn $ "Game variant was: " ++ show variant
            putStrLn $ "Tricks won:"
            let tricksByPlayer = map (\i -> length $ filter ((==i) . getTrickWinner (fromJust $ declaredVariant finalState)) (tricks finalState)) [0..2]
            mapM_ (\(i, count) -> putStrLn $ "Player " ++ show (i + 1) ++ ": " ++ show count ++ " tricks")
                  (zip [0..] tricksByPlayer)
        _ -> putStrLn "\nGame ended unexpectedly"
