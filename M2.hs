module M2

where

import GHC.Base
import System.Directory (getHomeDirectory)

-- Function which takes a list and sums only the even numbers

isEven :: Integer -> Bool
isEven = \x -> mod x 2 == 0

filterEven :: Integer -> Integer
filterEven = \x -> x * (1 - (mod x 2))

sumEven :: [Integer] -> Integer
sumEven = sum . g
          where g = map filterEven

sumEven1 :: [Integer] -> Integer
sumEven1 = sum . g                     -- sumEven1 = \xs -> sum (filter isEven xs)
           where g = filter isEven

qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (x:xs) = let smaller = [y | y <- xs, y <= x]
                   bigger = [y | y <- xs, y > x]
               in (qsort smaller) ++ [x] ++ (qsort bigger)

-- From http://www.cis.upenn.edu/~matuszek/cis554-2015/Assignments/haskell-1-exercises.html

member :: Eq a => a -> [a] -> Bool
--Returns True if the element occurs in the list.
member _ [] = False
member k (x:xs) = if k == x then True else member k xs

count :: (Num a, Eq a1) => a1 -> [a1] -> a
--Counts the number of times a value occurs in a list.
count k = sum . (map f)
          where f = \x -> if x == k then 1 else 0

forall :: (a -> Bool) -> [a] -> Bool
--Tests whether all elements of a list satisfy a given condition.
forall f xs = Prelude.foldr (&&) True (map f xs)
-- foldr Short circuits when first False is reached,
-- eg forall (\x -> if x == 10000000 then False else True) [1..]
-- foldl will not short circuit, hence will not terminate for infinite lists

exists :: (a -> Bool) -> [a] -> Bool
--Tests whether any element of a list satisfies a given condition.
exists f xs = Prelude.foldl (||) False (map f xs)
-- Same comment as for "forall"...foldr short circuits when first True is reached.

first :: (a -> Bool) -> [a] -> Maybe a
--Finds the first element of a list that satisfies a given condition.
first _ [] = Nothing
first f (x:xs) = if f x then (Just x) else first f xs

single :: (a -> Bool) -> [a] -> Bool
--Tests whether exactly one element of a list satisfies a given condition.
single f = let g = \x -> if f x then 1 else 0
           in \list -> if sum  (map g list) == 1 then True else False

mostly :: (a -> Bool) -> [a] -> Bool
--True if there are more elements in the list that satisfy the predicate than elements that fail to satisfy the predicate.
mostly f xs = count tlist > count flist
              where tlist = filter f xs
                    flist = filter (not . f) xs
                    count = sum . (map (\_ -> 1))

mostlyTrue :: [Bool] -> Bool
--True if the list contains more Trues than Falses (if equal, result is False).
mostlyTrue = mostly id
























