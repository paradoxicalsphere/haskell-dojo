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

{-
Rewrite the isWinningLine function using the built-in foldr function. The foldr function processes the input list
sequentially, one element at time, starting with the last (right-most) element in the list to the first (left-most).

The isWinningList__ function uses pattern matching to ensure that passing an empty line to the function returns False

In the lambda expression serving as the reducer function:

- x represents the current value retrieved from the input list
- y represents the current value of the accumulator variable

The foldr function initializes the accumulator variable to True (Boolean type). In the lambda expression, if the
current value retrieved from the input list does NOT match the Player value passed to the isWinningList__ function,
then the lambda expression returns False Each time the lambda expression finishes executing, the accumulator variable
is assigned the value that the lambda expression returns. If the accumulator variable is False, then the lambda
expression always returns False

NOTE: For more details on using the foldr function, see http://zvon.org/other/haskell/Outputprelude/foldr_f.html
-}
isWinningLine__ :: Player -> Line -> Bool
--isWinningLine__ inputPlayer__ inputLine__ = foldr (\x y -> if x == inputPlayer__ then True else False) False inputLine__
isWinningLine__ _ [] = False
isWinningLine__ inputPlayer__ inputLine__ = foldr (\x y -> if (x /= inputPlayer__) || not y then False else True) True inputLine__


-- Q#08

hasWon = undefined

-- Q#09

getGameState = undefined

playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined