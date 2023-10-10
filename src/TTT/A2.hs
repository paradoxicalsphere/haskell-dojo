module TTT.A2 where

import Data.List (intercalate)
import TTT.A1
import Data.Char

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

{-
The isDigit function returns True if the character passed into the function is a digit. Otherwise, the function returns False

NOTE: The isDigit function is included in the Data.Char module
-}
--isDigit :: Char -> Bool
--isDigit inputDigit = inputDigit `elem` ['0' .. '9']

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

-- Define an unlimited list of characters starting with 'A'
_UNLIMITED_CHAR_LIST_ :: [Char]
_UNLIMITED_CHAR_LIST_ = ['A', 'B'.. ]

{-
The zip function creates a list of tuples, where each tuple contains the elements appearing
in the same position in the two lists received as input

NOTE: While the number of elements in _UNLIMITED_CHAR_LIST_ is unlimited, the number of elements
in inputStringList is finite. The indexRowStrings function stops after reaching the end of the
list that the inputStringList variable contains.
-}
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings inputStringList = zip _UNLIMITED_CHAR_LIST_ inputStringList

-- Q#07

{-
Using a list of strings representing a row in the Tic Tac Toe board as input, separate
the contents of each square using the separator characters defined in the module TTT.A1
-}
formatLine :: [String] -> String
formatLine rowStringList = _PRE_SEP_ ++ intercalate _SEP_ rowStringList ++ _POST_SEP_

-- Q#08

-- The isMoveInBounds function returns True if the input tuple represents a valid square on a Tic Tac Toe board
isMoveInBounds :: Move -> Bool
isMoveInBounds inputMove =
  (fst inputMove >= 0 && fst inputMove <= 2) &&
  (snd inputMove >= 0 && snd inputMove <= 2)

-- Q#09

{-
Uncomment the following lines for testing:

stringToMove :: String -> Char
stringToMove inputString = head inputString

stringToMove :: String -> Int
stringToMove inputString = convertRowIndex (head inputString)

stringToMove :: String -> Char
stringToMove inputString = inputString !! 1
-}

{-
The stringToMove function accepts a string value using the format <Letter><Digit> to represent a square in the Tic Tac Toe board.
The function converts the string into a tuple of integer values representing the row and column of the square in the board.

If the format of the input string is not <Letter><Digit> then the function returns the integer -1 for the incorrect value,
as defined in the TTT.A1.convertRowIndex and TTT.A2.readDigit functions.

If the length of the input string is not 2, then the function returns the tuple (-1,-1) as defined in module TTT.A1

NOTE: Remember that in Haskell, a string is equivalent to a list of characters. In the stringToMove function, the where clause
deconstructs the value of inputString into head and tail components using the cons (:) constructor, assigning the head to the
variable x and the tail to the variable xs The head function returns the first character of an input string as type Char. The
tail function always returns the input string without the head character as type String, even when the tail is only one character
in length. To convert a one-character tail to the type Char, call the head function using the tail as input. Indexing a list--
for example, using <inputList> !! n to return the nth item in a list--is inefficient, even though indexing a string returns a
value of type Char as may be required.
-}
stringToMove :: String -> Move
stringToMove inputString
    | length inputString == 2 = (convertRowIndex x, readDigit (head xs))
    | otherwise = _INVALID_MOVE_
    where
      (x:xs) = inputString

-- | length inputString == 2 = (convertRowIndex (head inputString), readDigit (inputString !! 1))

-- Q#10

{-
Uncomment the following lines for testing:

replaceSquareInRow :: Player -> Int -> Row -> Player
replaceSquareInRow inputPlayerValue inputColumnIndex inputRow = inputPlayerValue

replaceSquareInRow :: Player -> Int -> Row -> Int
replaceSquareInRow inputPlayerValue inputColumnIndex inputRow = inputColumnIndex

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow inputPlayerValue inputColumnIndex inputRow = inputRow

replaceSquareInRow :: Player -> Int -> Row -> ([Square], [Square])
replaceSquareInRow inputPlayerValue inputColumnIndex inputRow = splitAt inputColumnIndex inputRow

replaceSquareInRow :: Player -> Int -> Row -> [Square]
replaceSquareInRow inputPlayerValue inputColumnIndex inputRow = drop inputColumnIndex inputRow
-}

{-
The replaceSquareInRow function accepts the following input:

- A value of type Square indicating the symbol for the player currently taking a turn
- A column in the Tic Tac Toe board represented using an Integer
- A row in the Tic Tac Toe board represented using a list containing values of type Square

The function returns a row represented using a list of type Square in which the symbol for
the player currently taking a turn replaces the value at the specified column in the row
passed into the function.

If the column value passed into the function is out of range, then the function returns
the input row unchanged.

NOTE: Guards are parsed in the order listed from top to bottom. If the null inputRow guard
appears lower in the list of guards, then passing an empty row into the replaceSquareInRow
function creates an error condition in the tail function.
-}
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow inputPlayerValue inputColumnIndex inputRow
    | null inputRow = inputRow
    | inputColumnIndex == 0 = inputPlayerValue : afterChangedColumn
    | inputColumnIndex == 1 = beforeChangedColumn ++ [inputPlayerValue] ++ afterChangedColumn
    | inputColumnIndex == 2 = beforeChangedColumn ++ [inputPlayerValue]
    | otherwise = inputRow
    where
      beforeChangedColumn = take inputColumnIndex inputRow
      afterChangedColumn = tail (drop inputColumnIndex inputRow)

{-
The rsX and rsO functions partially apply the replaceSquareInRow function. To call the functions
using the REPL, type the following commands:

rsX <ColumnIndex> <Row>

OR

rsO <ColumnIndex> <Row>
-}
rsX = replaceSquareInRow X
rsO = replaceSquareInRow O

