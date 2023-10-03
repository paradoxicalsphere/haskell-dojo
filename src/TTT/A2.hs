module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Completed October 3, 2023 to 

-- NOTES

{-
To avoid the warning -Wmissing-home-modules when using :r or :l TTT.A2 use :m TTT.A2 to load or reload the TTT.A2 module.
Alternately, use :m + TTT.A2 to load or reload the TTT.A2 module in addition to existing loaded modules. :m is
equivalent to :module For more details, see https://stackoverflow.com/questions/71172186/cabal-repl-cant-load-module-in-library
-}

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

_EMPTY_ROW_ = undefined

_EMPTY_BOARD_ = undefined

-- Q#05

isTied = undefined

_TIED_BOARD_ = undefined

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