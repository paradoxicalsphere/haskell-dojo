module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import qualified Main as recursion

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

formatRows :: [Row] -> [String]
formatRows = undefined

-- Q#04

isColEmpty = undefined

-- Q#05

dropFirstCol = undefined

dropLastCol = undefined

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