module H99_04 where

{-|
(*) Find the number of elements of a list.

Example in Haskell:

Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
-}

type MyLength a = [a] -> Int

myLength :: MyLength a
myLength a = length a

myLength' :: MyLength a
myLength' = length

-- The simple, recursive solution
myLength'' :: MyLength a
myLength'' (x:xs) = 1 + myLength'' xs
myLength'' [] = 0

-- Same, but using an "accumulator"
myLength''' :: MyLength a
myLength''' list = myLength_acc list 0
           where
                   myLength_acc [] n = n
                   myLength_acc (_:xs) n = myLength_acc xs (n+1)

-- Using foldl/foldr
-- TODO(adams): complete

solution04 = undefined

