module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import Foreign (toBool)

{-
NOTES

- In VSCodium, to identify where a function or constant is defined, hover over the function call or reference
using your mouse.


-}

-- Q#01

{-
The showInts function converts a list of integer values to a list of string values using recursion. The
recursive function named go appends the string version of the next item in the input list to an accumulator
list variable of type String named acc When all items in the input list of type Integer of are converted
to string values, then the function returns the list of items stored in the acc variable and stops.
-}
showInts :: [Int] -> [String]
showInts xs = go [] xs
    where
        go :: [String] -> [Int] -> [String]
        go acc (y:ys) = go (acc ++ [show y]) ys
        go acc [] = acc

-- The _HEADER_ constant displays a string used to label the columns in the Tic Tac Toe board
_HEADER_ :: String
_HEADER_ = " " ++ formatLine (showInts _RANGE_)

-- Q#02

{-
The showSquares function converts a list of items having type Square to a list of string values. The function
uses the same logic as the showInts function above to complete the conversion using recursion.
-}
showSquares :: [Square] -> [String]
showSquares xs = go [] xs
    where
        go :: [String] -> [Square] -> [String]
        go acc (y:ys) = go (acc ++ [showSquare y]) ys
        go acc [] = acc

-- Q#03

{-
The formatRows function converts a list of Row items into a list of String values. The function uses the same
logic as the showInts and showSquares functions to complete the conversion recursively.
-}
formatRows :: [Row] -> [String]
formatRows xs = go [] xs
    where
        go :: [String] -> [Row] -> [String]
        go acc (y:ys) = go (acc ++ [formatLine (showSquares y)]) ys
        go acc [] = acc

-- Q#04

{-
The following function also works:

isColEmpty :: Row -> Int -> Bool
isColEmpty (x:xs) inputColumn = if inputColumn == 0 then (if x == Void then True else False) else isColEmpty xs (inputColumn - 1)
isColEmpty [] _ = False
-}

{-
The isColEmpty function accepts a list of type Square and an integer as input. If the index in the list represented using the integer
contains the value Void, then the function returns the Boolean value True. Otherwise, the function returns the value False

The function recursively drops the head of the input list containing values of type Square until the column represented by the input
integer is at the head of the list. Then, the function tests the value of the head and returns the appropriate Boolean value based on
the comparison.

NOTE: The order of patterns is important. Patterns are parsed in order from top to bottom. Therefore, patterns are listed from specific
to general.
-}
isColEmpty :: Row -> Int -> Bool
isColEmpty (x:xs) 0 = if x == Void then True else False
isColEmpty (x:xs) inputColumn = isColEmpty xs (inputColumn - 1)
isColEmpty [] _ = False

-- Q#05

{-
The dropFirstCol function recursively scans the input variable having type Board one Row at a time, dropping the first item in each list
having type Row
-}
dropFirstCol :: Board -> Board
dropFirstCol xs = go [] xs
    where
        go :: Board -> Board -> Board
        go acc (y:ys) = go (acc ++ [tail y]) ys
        go acc [] = acc

{-
The dropLastCol function recursively scans the input variable having type Board one Row at a time, dropping the last item in each list
having type Row

NOTE: For more details on the init function, see http://zvon.org/other/haskell/Outputprelude/init_f.html
-}
dropLastCol :: Board -> Board
dropLastCol xs = go [] xs
    where
        go :: Board -> Board -> Board
        go acc (y:ys) = go (acc ++ [init y]) ys
        go acc [] = acc

-- Q#06

--NOTE: To test the getDiag1, getDiag2 and getAllLines functions, use the Board value [[X, O, Void], [X, O, X], [O, X, Void]]

{-
The getDiag1 function returns the values in a Tic Tac Toe board representing the diagonal line running from the top left corner
to the bottom right corner as a value having type Line, which is defined as a list of Square values.
-}
getDiag1 :: Board -> Line
getDiag1 xs = go [] xs
    where
        go :: Line -> Board -> Line
        go acc (y:ys) = go (acc ++ [head y]) (dropFirstCol ys)
        go acc [] = acc

{-
The getDiag2 function returns the values in a Tic Tac Toe board representing the diagonal line running from the top right corner
to the bottom left corner as a value having type Line

NOTE: For more details on the last function, see http://zvon.org/other/haskell/Outputprelude/last_f.html
-}
getDiag2 :: Board -> Line
getDiag2 xs = go [] xs
    where
        go :: Line -> Board -> Line
        go acc (y:ys) = go (acc ++ [last y]) (dropLastCol ys)
        go acc [] = acc

{-
The getAllLines function accepts a value of type Board as input, and then returns all the lines in the Board as a list.

NOTE: A Player wins the game when any line in the Board contains all X or all O values.
-}
getAllLines :: Board -> [Line]
getAllLines (x:xs) = [
    -- Export all rows in the Tic Tac Toe board from top to bottom
    x, head xs, last xs,
    -- Export all columns in the Tic Tac Toe board from left to right
    [ head x, head (head xs), head (last xs) ],
    [ head (tail x), head (tail (head xs)), head (tail (last xs)) ],
    [ last x, last (head xs), last (last xs) ],
    -- Export all diagonal lines in the Tic Tac Toe board
    getDiag1 (x:xs), getDiag2 (x:xs)
  ]
-- If the input Board is an empty list, then return an empty list
getAllLines [] = []

-- Q#07

{-
The putSquare function receives the following values as input:

- A Player value (X or O)
- A Tic Tac Toe board representing the current state of a game
- A tuple representing a square in the board using the format (<Row>, <Column>) having values in the range 0..2

The function returns the input board with the value in the square having the location indicated using the input tuple
with the input player value.

NOTE: Previous recursion examples in Q#01 through Q#06 implement either base and recursive patterns, or wrapper-worker
functions. In the putSquare function, the worker go function implements base and recursive patterns.
-}
putSquare :: Player -> Board -> Move -> Board
--The top level putSquare function servers as a wrapper that calls the worker go function
putSquare inputPlayer inputBoard inputMove = go [] inputPlayer inputBoard inputMove
    where
        go :: Board -> Player -> Board -> Move -> Board
        go acc inputPlayer (y:ys) (0 , inputMoveColumn) = acc ++ [replaceSquareInRow inputPlayer inputMoveColumn y] ++ ys
        go acc inputPlayer (y:ys) (inputMoveRow, inputMoveColumn) = go (acc ++ [y]) inputPlayer ys (inputMoveRow - 1, inputMoveColumn)
        go acc _ [] _ = []

-- Q#08

{-
The recursive prependRowIndices function labels each row in a Tic Tac Toe board with the letter that players use to specify the row
when making a move.
-}
prependRowIndices :: [String] -> [String]
prependRowIndices inputStringList = go [] (indexRowStrings inputStringList)
    where
        go :: [String] -> [(Char, String)] -> [String]
        go acc (y:ys) = go (acc ++ [concat [[fst y], ". ", snd y]]) ys
        go acc [] = acc

-- Q#09

{-
The isWinningLine function accepts as input a variable of type Player as well as a Line. If all values in the Line
match the value of the Player variable, then the function returns True. If any value in the Line does not match the
value of the Player variable, then the function returns false.

IMPORTANT: When the worker go function returns True, the function recursively continues testing then next value in
the input Line variable. If the worker go function returns False, then the function stops testing further values
in the Line variable and returns the False value. If the function would continue testing Line values after identifying
a value that does not match the Player value, then the function would incorrectly return True when testing the line
[O, X, O] to determine if Player O won, for example.

NOTE: The inputPlayer variable passed to the isWinningLine function is within the scope of the worker go function.
Therefore, explicitly passing the inputPlayer variable to the worker go function is not necessary.
-}
isWinningLine :: Player -> Line -> Bool
isWinningLine inputPlayer inputLine = go False inputLine
    where
        go :: Bool -> Line -> Bool
        go acc (y:ys) = if inputPlayer == y then go True ys else False
        go acc [] = acc

-- Q#10

{-
The isValidMove function receives a Board value and a Move value as input. In the Tic Tac Toe board received as input,
if the square indicated using the input Move value is empty, then the function returns True. If the indicated square
contains the value X or O, then the function returns False

If the input Board value is an empty list, or if the input Move value does not represent a valid square in a Tic Tac Toe
board, then the isValidMove function returns False

NOTE: To test the isValidMove function, use the board [[X,O,Void], [Void,Void,X], [Void,O,Void]] for example.
-}
isValidMove :: Board -> Move -> Bool
isValidMove inputBoard inputMove
    -- Use guards to ensure that the input Move value represents a valid square in a Tic Tac Toe board. The isMoveInBounds
    -- function serves as a wrapper function, calling the recursive worker go function.
    | isMoveInBounds inputMove = go inputBoard inputMove
    | otherwise = False
    where
        go :: Board -> Move -> Bool
        go (y:_) (0, goMoveColumn) = isColEmpty y goMoveColumn
        go (_:ys) (goMoveRow, goMoveColumn) = go ys (goMoveRow - 1, goMoveColumn)
        go [] _ = False