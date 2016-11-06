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

solution01 = undefined
