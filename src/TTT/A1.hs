module TTT.A1 where

import Data.Char (toUpper)
import GHC.Conc (BlockReason)
import Control.Concurrent (Chan)

-- Q#01

-- The following line is the type signature for the variable named _SIZE_
-- and declares _SIZE_ as an integer
_SIZE_ :: Int

-- Set value of the _SIZE_ variable to 3
_SIZE_ = 3

-- Q#02

-- The following signature declares the _DISPLAY_LOGO_ variable as a Boolean type
_DISPLAY_LOGO_ :: Bool

-- Intialize the value of _DISPLAY_LOGO_ to True
_DISPLAY_LOGO_ = True

-- Q#03

-- The convertToUpper function returns the character that the user types in upper case
-- To run the convertToUpper function, in the REPL type convertToUpper '<x>' where <x> is a character
convertToUpper :: Char -> Char
convertToUpper charToConvert = toUpper charToConvert

-- The convertToUnicode function returns the Unicode value for a character that the user types
-- To run the convertToUnicode function, in the REPL type convertToUnicode '<x>' where <x> is a character
convertToUnicode :: Char -> Int
convertToUnicode charToConvert = fromEnum charToConvert

-- The offsetUnicode function subtracts 65 from the Integer value that the user types
-- To run the offsetUnicode function, in the REPL type offsetUnicode <x> where <x> is an integer
offsetUnicode :: Int -> Int
offsetUnicode numberToOffset = numberToOffset - 65

-- To create a multiline comment, surround text using {- and -}

{-
The convertRowIndex function accepts a character as input. The function:
    1. Converts the character to upper case
    2. Converts the upper case character to the respective Unicode value
    3. Subracts 65 from the Unicode value

To run the convertRowIndex function, in the REPL type convertRowIndex '<x>' where <x> is a character
-}
convertRowIndex :: Char -> Int
convertRowIndex rowIndexValue = offsetUnicode (convertToUnicode (convertToUpper rowIndexValue))

-- Q#04

-- Define a tuple to represent an invalid row and column index
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05

-- Define a list of separator characters used to display the Tic Tac Toe grid
_SEP_ :: [[Char]]
_SEP_ = ["_", "|", "_"]

-- Q#06

{-
Define a data type named Square, with X, O and empty as the possible values for variables
having type Square. Allow the REPL to display Square values, and allow comparison between
values of type Square.
-}
data Square = X | O | Nothing
    deriving (Show, Eq)

-- Q#07

data GameState

-- Q#08

-- Q#09

getFirstPlayer = undefined

getFirstPlayer_ = undefined

-- Q#10

showGameState = undefined

-- Q#11

switchPlayer = undefined

-- Q#12

showSquare = undefined