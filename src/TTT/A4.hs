module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
--import TTT.A3 (getAllLines, putSquare)
import TTT.A3

{-
NOTES

- The commented out import statement above imports from module TTT.A3 selectively.
TTT.A4 rewrites some functions implemented in TTT.A3 To resolve dependency issues,
I moved some rewritten functions to TTT.A3 replacing the original implementations.

-}

-- Q#01

formatLineMap :: String -> String
formatLineMap inputString = _SEP_ ++ inputString

{-
Redefine the _HEADER_ constant using the built-in map function. The redefined _HEADER_ constant:

    1. Converts the Integers in the _RANGE_ list to a list of String values.
    2. Using the formatLineMap and built-in map functions, prepends the _SEP_ String value to each item in the list that step 1 returns.
    3. Appends a list item containing the _SEP_ String value to the list that step 2 returns.
    4. Using the built-in concat function, converts the list of String values that step 3 returns into a single String value.

-}
_HEADER_ :: String
_HEADER_ = concat $ map formatLineMap (showInts _RANGE_) ++ [_SEP_]

-- Q#02

-- showSquares = undefined

-- See TTT.A3 Q#02

-- Q#03

-- dropFirstCol = undefined

-- See TTT.A3 Q#05

-- Q#04

-- dropLastCol = undefined

-- See TTT.A3 Q#05

--Q#05

formatRows = undefined

-- Q#06

isWinningLine_ = undefined

-- Q#07

isWinningLine = undefined

-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined

playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined