module HM.A6 where

import Data.Char (isAlpha)
import HM.Provided

-- Q#01

-- Define type synonyms required for the Hangman game
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02

-- Define a custom data type used in error handling
data GameException = InvalidChars | InvalidLength | NotInDict | InvalidMove | RepeatMove | GameOver
    deriving (Show, Eq)

-- Q#03

{-
Define a constant used to determine the minimum and maximum lengths of a string having type Secret,
where the first item in the tuple represents the minimum length and the second item in the tuple
represents the maximum length.
-}
_LENGTH_RANGE_ :: (Int, Int)
_LENGTH_RANGE_ = (4, 7)

lengthInRange :: Secret -> Bool
lengthInRange inputSecret = if (inputSecretLength >= minLength) && (inputSecretLength <= maxLength) then True else False
    where
        inputSecretLength = length inputSecret
        minLength = fst _LENGTH_RANGE_
        maxLength = snd _LENGTH_RANGE_

-- Q#04

{-
The invalidMove function ensures that characters users type as input to the Hangman game are letters. The function returns
True if the input character is NOT a letter. If the input character is a letter, then the function returns the value False
 -}
invalidMove :: Move -> Bool
invalidMove inputMove = not $ isAlpha inputMove

-- Q#05

{-
The revealLetters receives a letter, a secret word and a partially revealed version of the secret word as input. If the
input letter matches any letter in the secret word, then the letter is revealed in the partially revealed version of the
secret word.

More specifically, the zipWith function parses the two input lists, applying the defined lambda function to the items in
the same position from each list, one pair of items at a time, to produce output comprised of a single list.
-}
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters inputMove inputSecret inputGuess = zipWith (\x y -> if inputMove == x then x else y) inputSecret inputGuess

{-
-- The following interim testing function works
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters inputMove inputSecret inputGuess = go [] inputSecret inputGuess
    where
        go :: Guess -> Secret -> Guess -> Guess
        go acc (y:ys) (z:zs) = if inputMove == y then go (acc ++ [y]) ys zs else go (acc ++ [z]) ys zs
        go acc [] [] = acc

        -- Implement exhaustive pattern matching, according to the compiler
        go [] [_] [] = "Error 1"
        go [_] [_] [] = "Error 2"
        go [_, _] [_] [] = "Error 3"
        go (_:_:_:_) [_] [] = "Error 4"

        go [] [] [_] = "Error 5"
        go [_] [] [_] = "Error 6"
        go [_, _] [] [_] = "Error 7"
        go (_:_:_:_) [] [_] = "Error 8"

        go [] (_:_:_) [] = "Error 9"
        go (_:_) (_:_:_) [] = "Error 10"
        go [] [] (_:_:_) = "Error 11"
        go (_:_) [] (_:_:_) = "Error 12"
-}

{-
-- The following interim testing function works
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters inputMove inputSecret inputGuess = initialInputGuess
    where
        inputSecretLength = length inputSecret
        initialInputGuess = replicate inputSecretLength '_'
-}

{-
-- The following interim testing function works and is case sensitive
ifCharsEqual :: Char -> Char -> Bool
ifCharsEqual firstInputChar secondInputChar = if firstInputChar == secondInputChar then True else False
-}

-- Q#06

updateChances = undefined

-- Q#07

setSecret = undefined

