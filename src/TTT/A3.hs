module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

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

getDiag1 = undefined

getDiag2 = undefined

getAllLines = undefined

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined