module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Completed October 3, 2023 to 

-- NOTES

-- When typing :l TTT.A2 or :r ignore the warning -Wmissing-home-modules

-- Q#01

-- Define a function that constructs and returns a string used to prompt the current player to type a move
constructPlayerPrompt :: String -> String
constructPlayerPrompt currentPlayerString = concat ["PLAYER ", currentPlayerString, "'S TURN - Please type a row and column position (for example, A1): "]

-- Define a function that displays the string prompting the current player to type a move
promptPlayer :: Player -> String
promptPlayer currentPlayer = constructPlayerPrompt (showSquare currentPlayer)

-- Q#02

{-
Define a list of integers used to test that players type row and column positions that exist on a Tic Tac Toe board

NOTE: The upper limit of the range is defined using the _SIZE_ constant defined in module TTT.A1
-}
_RANGE_ :: [Int]
--_RANGE_ = [0, 1, 2]
_RANGE_ = [0..(_SIZE_ - 1)]

-- Q#03

-- The isDigit function returns True if the character passed into the function is a digit. Otherwise, the function returns False
isDigit :: Char -> Bool
isDigit inputDigit = inputDigit `elem` ['0' .. '9']

-- The convertCharToString function converts a character into a string
convertCharToString :: Char -> String
convertCharToString inputChar = [inputChar]

-- The convertStringToInt function converts a string into an integer
convertStringToInt :: String -> Int
convertStringToInt inputString = read inputString :: Int

{-
The readDigit function converts an input character into the corresponding integer, if the character is a digit.
Otherwise, the function returns the integer -1
-}
readDigit :: Char -> Int
readDigit inputDigit = if isDigit inputDigit then convertStringToInt (convertCharToString inputDigit) else (-1)

-- Q#04

-- Initialize a list of three empty variables having type Square representing a row in the Tic Tac Toe board
_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate 3 Void

{-
To represent the Tic Tac Toe board, initialize a list of three variables, each variable being
a list of type Square containing three empty values
-}
_EMPTY_BOARD_ :: [[Square]]
_EMPTY_BOARD_ = [ _EMPTY_ROW_, _EMPTY_ROW_, _EMPTY_ROW_ ]

-- Q#05

{-
The isTied fuction tests the state of a Tic Tac Toe board and returns True if the board contains
no empty squares. If the board contains an empty square, then the function returns False

NOTE: Module TTT.A1 defines the Board synomym in Q#08. The concat function concatenates
a list of lists into a list. The notElem function requires a list as input.
-}
isTied :: Board -> Bool
isTied inputBoard = Void `notElem` concat inputBoard

{-
Initialize a constant containing a board state used to test the isTied function.

NOTE: You can also use the _EMPTY_BOARD_ constant to test the isTied function.
-}
_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [ X, O, X ],
    [ O, X, O ],
    [ O, X, O ]
  ]

-- Q#06

indexRowStrings = undefined

-- Q#07

formatLine = undefined

-- Q#08

isMoveInBounds = undefined

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined