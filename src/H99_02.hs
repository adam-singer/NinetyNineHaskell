module H99_02 where

{-|
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}

myButLast :: [a] -> a
-- myButLast (x:[])
myButLast xs = xs !! (length xs - 2)

myButLast' :: [a] -> a
myButLast' (x:[]) = error "list does not have more then one element"
myButLast' (x:y:[]) = x
myButLast' (x:xs) = myButLast' xs

{-|
λ > :doc init
init :: [a] -> [a]
base Prelude
Return all the elements of a list except the last one. The list must
be non-empty.

λ > :doc last
last :: [a] -> a
base Prelude
Extract the last element of a list, which must be finite and
non-empty.

λ > :t last . init
last . init :: [c] -> c
-}
myButLast'' :: [a] -> a
myButLast'' = last . init

{-|
λ > :doc reverse
reverse :: [a] -> [a]
base Prelude
reverse xs returns the elements of xs in
reverse order. xs must be finite.
-}

myButLast''' :: [a] -> a
myButLast''' xs = reverse xs !! 1

myButLast'''' :: [a] -> a
myButLast'''' (x:_:[]) = x
myButLast'''' (_:xs) = myButLast'''' xs

myButLast''''' :: [a] -> a
myButLast''''' = head . tail . reverse

myButLast'''''' :: Foldable f => f a -> a
myButLast'''''' = fst . foldl (\(a,b) x -> (b,x)) (err1, err2)
  where
    err1 = error "Empty list"
    err2 = error "Singleton"

--myButLast''''''' = fst . foldl carrySnd errs
--  where
--    carrySnd (a, b) x = (b, x)
--    errs = (error "Empty list", error "Singleton")

myButLast'''''''' :: Foldable f => f a -> Maybe a
myButLast'''''''' = fst . foldl carrySndMaybe emptyOrSingleton
  where
    carrySndMaybe (a, b) x = (b, Just x)
    emptyOrSingleton = (Nothing, Nothing)

myButLast''''''''' [] = error "empty list"
myButLast''''''''' [x] = error "singleton list"
myButLast''''''''' (x:xs) =
  if length xs == 1 then x
                    else myButLast''''''''' xs
