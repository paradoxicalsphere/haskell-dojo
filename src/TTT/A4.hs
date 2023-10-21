module TTT.A4 where

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

{-
formatLineMap :: String -> String
formatLineMap inputString = _SEP_ ++ inputString
-}

{-
Redefine the _HEADER_ constant using the built-in map function. The redefined _HEADER_ constant:

    1. Converts the Integers in the _RANGE_ Integer list to a list of String values prepended with the _SEP_ separator String constant
    2. Appends a list item containing the _SEP_ String value to the list that step 1 returns.
    3. Using the built-in concat function, converts the list of String values that step 3 returns into a single String value.
-}
_HEADER_ :: String
-- _HEADER_ = concat $ map formatLineMap (showInts _RANGE_) ++ [_SEP_]
-- _HEADER_ = formatLine (showInts _RANGE_)
_HEADER_ = concat $ map (\x -> _SEP_ ++ show x) _RANGE_ ++ [_SEP_]

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

-- Rewrite the formatRows function using map
formatRows :: Board -> [String]
formatRows inputBoard = map (\x -> formatLine (showSquares x)) inputBoard

-- Q#06

-- Rewrite isWinningLine using the built-in filter function
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ inputPlayer_ inputLine_ = if not (null inputLine_) && null (filter (/=inputPlayer_) inputLine_) then True else False

-- Q#07

isWinningLine__ = undefined

-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined

playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined