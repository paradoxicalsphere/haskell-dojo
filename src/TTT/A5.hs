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
To identify the encoding for a file, using a Terminal window type file <FileName>

IMPORTANT: The original ttt-logo.txt file contains an encoding issue that raises the following
error when attempting to load the file in Haskell:

*** Exception: ./assets/ttt-logo.txt: hGetContents: invalid argument (invalid byte sequence)

To fix the issue, using a Terminal window type iconv -f UTF-8 -t UTF-8 ./ttt-logo.txt.original -o ttt-logo.txt

NOTE: For more details on using iconv, see https://www.tecmint.com/convert-files-to-utf-8-encoding-in-linux/
-}

-- The ttt-logo.txt file uses UTF-8 encoding
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

-- The ttt-logo.txt.ascii file uses ASCII encoding
_LOGO_PATH_ASCII_ :: FilePath
_LOGO_PATH_ASCII_ = "./assets/ttt-logo.txt.ascii"

-- The utf8-demo.txt file is available for download from https://antofthy.gitlab.io/info/data/utf8-demo.txt
_LOGO_PATH_DEMO_ :: FilePath
_LOGO_PATH_DEMO_ = "./assets/utf8-demo.txt"

{-
For more details on the following implementation of printLogo, see:

https://stackoverflow.com/questions/33444796/read-file-with-utf-8-in-haskell-as-io-string
-}
printLogo :: IO ()
printLogo = do
    inputHandle <- openFile _LOGO_PATH_ ReadMode
    hSetEncoding inputHandle utf8
    theText <- hGetContents inputHandle
    putStrLn theText

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

firstPlayer = undefined

-- Q#04

getMove = undefined

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