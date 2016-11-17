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
myLength'''' :: MyLength a
myLength'''' = foldl (\n _ -> n + 1) 0
myLength''''' :: MyLength a
myLength''''' = foldr (\_ n -> n + 1) 0
myLength'''''' :: MyLength a
myLength'''''' = foldr (\_ -> (+1)) 0
myLength''''''' :: MyLength a
myLength''''''' = foldr ((+) . (const 1)) 0
myLength'''''''' :: MyLength a
myLength'''''''' = foldr (const (+1)) 0
myLength''''''''' :: MyLength a
myLength''''''''' = foldl (const . (+1)) 0

-- Zipping with an infinite list
-- Create infinite list starting from 1 then zip the two lists together and take the last
-- element which is a pair from the result.

myLength'''''''''' xs = snd $ last $ zip xs [1..]
myLength''''''''''' = snd . last . (flip zip [1..])
myLength'''''''''''' = fst . last . zip [1..]

-- Mapping all elements to "1"
myLength''''''''''''' = sum . map (\_ -> 1)

solution04 = undefined

