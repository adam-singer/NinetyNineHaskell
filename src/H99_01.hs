module H99_01 where

{-|
(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}

myLast :: [a] -> a
myLast [] = error "list is empty"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

{-|

λ > :t const
const :: a -> b -> a
λ > :t const id
const id :: b -> a -> a
λ > :t foldr1 (const id)
foldr1 (const id) :: Foldable t => t b -> b
λ > :t foldr1 (const id) $ [1,2,3]
foldr1 (const id) $ [1,2,3] :: Num b => b
λ > foldr1 (const id) $ [1,2,3]
    3
it :: Num b => b

-}

myLast' :: [a] -> a
myLast' = foldr1 (const id)

myLast'':: [a] -> a
myLast'' = head . reverse

{-|
λ > :t const
const :: a -> b -> a
λ > :t flip
flip :: (a -> b -> c) -> b -> a -> c
λ > :t foldr1
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
λ > :t flip const
flip const :: b -> c -> c
λ > :t foldr1 (flip const)
foldr1 (flip const) :: Foldable t => t b -> b
λ > :t foldr1 (flip const) [1,2,3]
foldr1 (flip const) [1,2,3] :: Num b => b
λ > foldr1 (flip const) [1,2,3]
    3
it :: Num b => b
-}

myLast''' :: [a] -> a
myLast''' = foldr1 (flip const)

{-|

λ > :t snd
snd :: (a, b) -> b
λ > :t curry
curry :: ((a, b) -> c) -> a -> b -> c
λ > :t foldl1
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
λ > :t foldl1 (curry snd)
foldl1 (curry snd) :: Foldable t => t a -> a
λ > foldl1 (curry snd) [1,2,3]
    3
it :: Num a => a

-}

myLast'''' :: [a] -> a
myLast'''' = foldl1 (curry snd)

{-|

-}

myLast''''' :: [a] -> a
myLast''''' [] = error "list is empty"
myLast''''' xs = xs !! (length xs - 1)

myLast'''''' :: [a] -> a
myLast'''''' = last

