> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f z [] = z
> myFoldl f z (x:xs) = let z' = z `f` x
> in myFoldl f z' xs


Next, define a function to reverse a list using foldl.

> myReverse :: [a] -> [a]
> myReverse = myFoldl (flip(:)) []


Now define your own version of foldr, named myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f z [] = z
> myFoldr f z (x:xs) = x `f` myFoldr f z xs


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?

Instead of foldl, try using foldl'.
Why is it faster?
(Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)


For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.

Next, using the map function, convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs x = map abs x

Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs x = sum $ map abs x