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

- If typing :l TTT.A4 for example does not work, then type :r

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

--Use the following constant definitions to test the hasWon function
_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

{-
The hasWon function receives a Player value and a Board value as input. The function evaluates all lines in a Tic Tac Toe board
and returns True if any line contains only values matching the in put Player value. Otherwise, the function returns False

The foldr function receives as input a list of Line values representing all lines in the Tic Tac Toe board. Each line value is
comprised of a list of Square values. The isWinningLine_ function received a Player value and Line value as input, returning
True if each Square value in the Line matches the input Player value. The lambda function tests each line in the Tic Tac Toe
board, returning True if any line is a winning line.
-}
hasWon :: Player -> Board -> Bool
hasWon inputPlayer inputBoard = foldr (\x y -> if not y then isWinningLine_ inputPlayer x else True) False (getAllLines inputBoard)

-- Q#09

{-
The getGameState function receives a Tic Tac Toe board as input. The function evaluates the board, and then returns a value
having type GameState indicating the current state of the Tic Tac Toe game.
-}
getGameState :: Board -> GameState
getGameState inputBoard
    | hasWon X inputBoard = XWon
    | hasWon O inputBoard = OWon
    | isTied inputBoard = Tie
    | otherwise = InProgress

{-
The playMove function updates the square indicated by the input Move value in the Tic Tac Toe board that the input Board
value provides with the Player value that the function also receives as input.

The function returns a tuple containing the current state of the Tic Tac Toe game in the first element of the tuple, and the
updated Tic Tac Toe board in the second element of the tuple.

IMPORTANT: The playMove function does NOT ensure that the square the function updates contains the value Void
-}
playMove :: Player -> Board -> Move -> (GameState, Board)
playMove inputPlayer inputBoard inputMove = (getGameState updatedBoard, updatedBoard)
    where
        updatedBoard = putSquare inputPlayer inputBoard inputMove

-- Q#10

{-
Rewrite prependRowIndices using the built-in zipWith function.

For more details on the zipWith function, see http://zvon.org/other/haskell/Outputprelude/zipWith_f.html

The following version works using a finite list of String values prepended to the respective item in the input String list.
-}
{-
_LIMITED_STRING_LIST_ :: [String]
_LIMITED_STRING_LIST_ = ["A", "B", "C"]

prependRowIndices :: [String] -> [String]
prependRowIndices inputStringList = zipWith (++) _LIMITED_STRING_LIST_ inputStringList
-}

{-
The following version uses the _UNLIMITED_CHAR_LIST_ constant defined in the module TTT.A2

NOTE: The original version of the prependRowIndices function defined in module TTT.A3 Q#08 also uses the
_UNLIMITED_CHAR_LIST_ constant.

The number of items in the inputStringList variable is used to select the number of characters retrieved from the
_UNLIMITED_CHAR_LIST_ constant, which is defined as having an indeterminate range. The map function converts a
String value into a list of String values, each having length 1 For more details on the specific map function, see
https://copyprogramming.com/howto/string-to-list-of-characters
-}
prependRowIndices :: [String] -> [String]
prependRowIndices inputStringList = zipWith (++) (map (:[]) $ take (length inputStringList) _UNLIMITED_CHAR_LIST_) inputStringList

-- Q#11

formatBoard = undefined