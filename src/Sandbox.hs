module Sandbox where

-- 231003 - Explore the Unit type
x :: ()
x = ()

type Empty = ()

data Square = A | B | Empty
    deriving (Show, Eq)

