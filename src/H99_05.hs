module H99_05 where

{-|
(*) Reverse a list.

Example in Haskell:

Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
-}

type MyReverse a = [a] -> [a]
myReverse :: MyReverse a
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

myReverse1 xs = foldr (\a b -> b ++ [a]) [] xs

-- standard definition
myReverse2 xs = foldl (flip (:)) []

myReverse3 xs = myReverse3' xs []
  where
    myReverse3' [] reversed = reversed
    myReverse3' (x:xs) reversed = myReverse3' xs (x:reversed)

myReverse4 xs = foldr (\x fId empty -> fId (x:empty)) id xs []

solution05 = undefined

