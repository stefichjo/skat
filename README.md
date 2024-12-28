# Skat Card Game

A Haskell implementation of the traditional German card game Skat.

## Overview

Skat is a three-player trick-taking card game that uses a deck of 32 cards. This implementation provides the core game mechanics and rules of Skat.

## Building and Running

To build and run the game, you'll need GHC (Glasgow Haskell Compiler) and cabal installed. Then:

```bash
cabal build
cabal run
```

## Project Structure

- `src/Main.hs`: Entry point and game loop
- `src/Card.hs`: Card types and deck management
- `src/Player.hs`: Player representation and actions
- `src/Game.hs`: Core game logic and state management

## Game Rules

Skat is played with:
- 3 players
- 32 cards (7 through Ace in four suits)
- 2 cards set aside as the "Skat"

The game consists of three phases:
1. Bidding
2. Picking up Skat and declaring game type
3. Playing tricks

## Current Status

Basic implementation with:
- Card and deck representation
- Player management
- Game state tracking
- Initial game setup

Future improvements planned:
- Complete bidding mechanics
- Trick-taking implementation
- Scoring system
- Game variations (Grand, Null, etc.)
