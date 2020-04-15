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
