module TTT.A1 where

import Data.Char (toUpper)
import GHC.Conc (BlockReason)
import Data.Void (Void)

-- Module TTT.A1 completed September 28, 2023 to October 3, 2023

-- NOTES

-- To start an instance of the REPL, type cabal repl in the VSCodium Terminal area.
-- To load the current module, type :l TTT.A1 in the REPL
-- To reload a module in the REPL, type :r or :reload

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

{-
The convertToUpper function returns the character that the user types in upper case

To run the convertToUpper function, in the REPL type convertToUpper '<x>' where <x> is a character

NOTE: Using single quotes (') indicates a Char type. Using double quotes (") indicates a list
of characters, also expressed as [Char] The brackets ([ and ]) indicate a list. In Haskell,
a list of characters is equivalent to a string.
-}
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

NOTE: In the REPL, you may also use parentheses to calculate and delineate input values
passed to functions. For example, typing offsetUnicode (25 + 75) returns 35
-}
convertRowIndex :: Char -> Int
convertRowIndex rowIndexValue = offsetUnicode (convertToUnicode (convertToUpper rowIndexValue))

-- Q#04

{-
Define a tuple to represent an invalid row and column index

NOTE: In Haskell, a collection having a pair or trio (triple) of values is expressed as a
tuple using parentheses ( and ) Each value in a tuple may have a different data type.
Collections having more than two values are expressed using a list. To delineate list,
use brackets [ and ] In a list, each element has the same data type.
-}
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05

-- Define a list of separator characters used to display the Tic Tac Toe grid
--_SEP_ :: [Char]
--_SEP_ = ['_', '|', '_']

_SEP_ :: String
_SEP_ = "_|_"

-- Q#06

{-
Define a data type named Square, with X, O and empty as the possible values for variables
having type Square. Allow the REPL to display Square values, and allow comparison between
values of type Square.

IMPORTANT: The Unit type is essentially an empty tuple and is written as () The Unit type
is similar to Void For more detail, see https://stackoverflow.com/questions/16892570/what-is-in-haskell-exactly
For examples exploring the Unit type, see the module named Sandbox

NOTE: A typeclass defines a set of methods that is shared across multiple types. You may
define default implementations for all basic typeclasses, for example you can implement
deriving (Show, Read, Eq, Ord, Bounded, Enum)
-}
data Square = X | O | Void
    deriving (Show, Eq)

-- Q#07

{-
Define a data type named GameState, with XWon, OWon, Tie and InProgress as the possible
values for variables having the type GameState. Allow the REPL to display GameState values,
and allow comparison between values of type GameState.
-}
data GameState = XWon | OWon | Tie | InProgress
    deriving (Show, Eq)

-- Q#08

-- Define type synomyms. Brackets ([ and ]) indicate a list.
type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09

-- The getFirstPlayer function uses an if...then...else statement to return X if the input value is True
-- and return O if the input value is False
getFirstPlayer :: Bool -> Player
getFirstPlayer booleanValue = if booleanValue then X else O

{-
The getFirstPlayer_ function uses guards to create the same logic as the getFirstPlayer function

NOTE: In guards, case expressions and pattern matching, checks are completed sequentially, in
order from top to bottom, or first to last.
-}
getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ booleanValue | booleanValue = X
                             | not booleanValue = O
                             | otherwise = Void

-- Q#10

-- The showGameState function receives a GameState value as input, and then returns a string describing
-- the game state based on the input value.
showGameState :: GameState -> String
showGameState gameStateValue = case gameStateValue of
    XWon -> "X Won!"
    OWon -> "O Won!"
    Tie -> "The game has ended in a tie."
    InProgress -> "The game is still in progress..."

-- Q#11

-- Implement pattern matching to execute a different switchPlayer function based on the input value
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer Void = Void

-- Q#12

-- Implement the showSquare function to return a string value representing the value of type Square
-- that the function receives as input
showSquare :: Square -> String
showSquare squareValue = case squareValue of
    X -> "X"
    O -> "O"
    Void -> "_"