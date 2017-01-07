module H99_06 where

import qualified Control.Monad as M (liftM2)
import qualified Control.Applicative as A ((<*>))
import qualified Control.Arrow as AR ((&&&))

{- |
(*) Find out whether a list is a palindrome.
A palindrome can be read forward or backward;
e.g. (x a m a x).

Example in Haskell:

*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Do comparison from outside to in
isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

{-|
λ > zip "abc" (reverse "abc")
    [('a', 'c'), ('b', 'b'), ('c', 'a')]
it :: [(Char, Char)]
λ > zip "abc" (reverse "aba")
    [('a', 'a'), ('b', 'b'), ('c', 'a')]
it :: [(Char, Char)]
λ > zip "aba" (reverse "aba")
    [('a', 'a'), ('b', 'b'), ('a', 'a')]
-}
isPalindrome'' :: Eq a => [a] -> Bool
isPalindrome'' xs = foldl (\acc (a, b) -> if a == b then acc else False) True input
  where
  input = zip xs (reverse xs)


isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = M.liftM2 (==) id reverse

{-
λ > :i A.<*>
class Functor f => Applicative (f :: * -> *) where
  ...
  (<*>) :: f (a -> b) -> f a -> f b
  ...
        -- Defined in ‘GHC.Base’
infixl 4 <*>
-}
isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' = (==) A.<*> reverse

{-
In this case `isPal` is the driver function defined after `where`.
-}
isPalindrome''''' :: (Eq a) => [a] -> Bool
isPalindrome''''' xs = isPal [] xs xs
  where isPal rev (x:xs) (_:_:ys) = isPal (x:rev) xs ys
        isPal rev (x:xs) [_] = rev == xs
        isPal rev xs [] = rev == xs

{-
λ > zipWith (==) "abc" (reverse "abc")
    [False, True, False]
it :: [Bool]
λ > zipWith (==) "aba" (reverse "aba")
    [True, True, True]
it :: [Bool]
 -}
isPalindrome'''''' :: (Eq a) => [a] -> Bool
isPalindrome'''''' xs = foldr (&&) True $ zipWith (==) xs (reverse xs)

isPalindrome''''''' :: (Eq a) => [a] -> Bool
isPalindrome''''''' xs = and $ zipWith (==) xs (reverse xs)

isPalindrome'''''''' :: (Eq a) => [a] -> Bool
isPalindrome'''''''' xs = take halfLen xs == reverse ((drop (halfLen + (len `mod` 2))) xs)
  where len = length xs
        halfLen = len `div` 2

isPalindrome''''''''' :: (Eq a) => [a] -> Bool
isPalindrome''''''''' xs = leftSide == reverse rightSide
  where len = length xs
        halfLen = len `div` 2
        (leftSide, rightSide') = splitAt halfLen xs
        rightSide = drop (len `mod` 2) rightSide'

-- With monomorphism restriction
isPalindrome'''''''''' :: (Eq a) => [a] -> Bool
isPalindrome'''''''''' xs = (uncurry (==) . (id AR.&&& reverse)) xs

-- Point free with no monomorphism restriction
isPalindrome''''''''''' :: (Eq a) => [a] -> Bool
isPalindrome''''''''''' = uncurry (==) . (id AR.&&& reverse)
