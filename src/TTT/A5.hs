module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

-- The System.IO module is required for the printLogo function using openFile in Q#02 below
import System.IO

-- Q#01

printBoard :: Board -> IO ()
printBoard inputBoard = putStrLn $ formatBoard inputBoard

-- Q#02

{-
To identify the encoding for a file, using a Terminal window type:

file <FileName>
-}

-- The ttt-logo.txt file uses UTF-8 encoding
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

-- The ttt-logo.txt.ascii file uses ASCII encoding
_LOGO_PATH_ASCII_ :: FilePath
_LOGO_PATH_ASCII_ = "./assets/ttt-logo.txt.ascii"

-- The utf8-demo.txt file is available for download from https://antofthy.gitlab.io/info/data/utf8-demo.txt
--_LOGO_PATH_DEMO_ :: FilePath
--_LOGO_PATH_DEMO_ = "./assets/utf8-demo.txt"

{-
For more details on the following implementation of printLogo, see:

https://stackoverflow.com/questions/33444796/read-file-with-utf-8-in-haskell-as-io-string
-}
printLogo :: IO ()
printLogo = do
    -- Create a handle to the text file containing the logo art
    inputHandle <- openFile _LOGO_PATH_ ReadMode
    -- Indicate the encoding of the text file containing the logo art
    hSetEncoding inputHandle utf8
    -- Retrieve the contents of the text file containing the logo art
    inputLogo <- hGetContents inputHandle
    -- Set the encoding to use for displaying the logo in the Terminal window
    hSetEncoding stdout utf8
    -- Display the logo in the Terminal window
    putStrLn inputLogo

{-
The following implementation of printLogoASCII does NOT support input files using UTF-8 encoding.
Specifically, the readFile built-in function does NOT support UTF-8 encoding.

For more details, see:

https://wiki.haskell.org/Tutorials/Programming_Haskell/String_IO
https://stackoverflow.com/questions/8578578/writing-an-io-string-to-stdout-in-haskell
https://serokell.io/blog/haskell-with-utf8
https://stackoverflow.com/questions/7371978/haskell-read-in-special-characters-from-console
-}
{-
printLogoASCII :: IO ()
printLogoASCII = do
    inputLogo <- readFile _LOGO_PATH_ASCII_
    putStrLn inputLogo
-}

-- Q#03

_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

{-
firstPlayer :: IO Bool
firstPlayer = do
     _RANDOM_BOOL_
-}

{-
The firstPlayer monad function:

1. Assigns a random Boolean value to the randomBool variable.
2. Passes the random Boolean value as an input parameter to the getFirstPlayer function.
3. Returns the output value of the getFirstPlayer function to the IO monad context for display.

NOTE: The getFirstPlayer function receives a value of type Bool and returns a value of type Player

NOTE: For more details on using the IO monad, see https://www.haskell.org/tutorial/io.html
-}
firstPlayer :: IO Player
firstPlayer = do
    randomBool <- _RANDOM_BOOL_
    return (getFirstPlayer randomBool)

-- Q#04

{-
The getMove function:

1. Receives a Board value as input
2. Prompts the user for a String value representing a square in the Tic Tac Toe board using the format
   <RowLetter><ColumnNumber> where <RowLetter> is a letter from A to C and <ColumnNumber> is an integer
   from 0 to 2
3. Converts the string value received in step 2 to a value of type Move
4. Tests if the square indicated using the Move value in step 3:
      a) Is within the bounds of a Tic Tac Toe board
      b) Represents an empty square in the Tic Tac Toe board received in step 1
5. If the move is valid as determined in step 4, then the function displays the tuple representing the
   square in the Tic Tac Toe board that the user typed in step 2

   OR

   If step 4 determines that the move is NOT valid, then the getMove function displays a message informing
   the user before recursively calling the getMove function again.
-}
getMove :: Board -> IO Move
getMove inputBoard = do
    inputString <- getLine
    let inputMove = stringToMove inputString
    let moveIsValid = isValidMove inputBoard inputMove
    if moveIsValid then
        return inputMove
    else
        do
            putStrLn "Invalid move! Try again..."
            getMove inputBoard

-- Q#05

play = undefined

-- Q#06

runTTT :: IO ()
runTTT = putStrLn "Not implemented... yet!"

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined