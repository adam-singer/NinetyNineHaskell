module H99_03 where

{-|
(*) Find the K'th element of a list. The first element in the list is number 1.

Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'

-}

type ElementAt a = [a] -> Int -> a

elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i-1)

elementAt' :: ElementAt a
elementAt' = \xs -> \i -> xs !! (i-1)

elementAt'' :: ElementAt a
elementAt'' (x:xs) i = if (i <= 0) then x else (elementAt'' xs (i-1))
elementAt'' [] _ = error "List empty or element not found"

elementAt''' :: ElementAt a
elementAt''' (x:xs) 1 = x
elementAt''' [] _ = error "Index out of bounds"
elementAt''' (_:xs) k
  | k < 1     = error "Index out of bounds"
  | otherwise = elementAt''' xs (k - 1)

elementAt'''' :: ElementAt a
elementAt'''' (x:_) 1 = x
elementAt'''' (_:xs) i = elementAt'''' xs (i - 1)
elementAt'''' _ _ = error "Index out of bounds"
-- Will not work in inf lists with invalid indexes elementAt'''' [1..] 0

elementAt''''' :: ElementAt a
elementAt''''' xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = fst . last $ zip xs [1..n]

elementAt'''''' :: ElementAt a
elementAt'''''' xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = head . reverse $ take n xs

elementAt''''''' :: ElementAt a
elementAt''''''' xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = head $ drop (n - 1) xs

elementAt_pf = (last .) . take . (+ 1)
elementAt_pf' = flip $ (last .) . take . (+ 1)

solution03 = undefined
