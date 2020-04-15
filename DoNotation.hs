#!/usr/bin/env stack
-- stack --install-ghc runghc

module DoNotation
(
  Crispy(..),
  fibs1,
  fibs2
) where

import Data.List
import qualified Data.Map as M hiding ((>>=))
import qualified Data.Vector as V hiding ((>>=))
import Data.Functor
import Data.Monoid
import GHC.Base hiding (mapM, sequence, (>>=))
import Control.Monad
--import Control.Comonad
--import Control.Monad.Random
import System.Random

main :: IO ()
main = do
           putStrLn "Hello World! What is your name?";
           name <- getLine;
           putStrLn $ "Hi " ++ name ++ " :) Please enter a number:";
           n <- getLine;
           print $ show $ take (read n) [1..];
           print . show . take (read n) $ [1..];
           do{
             let num = take (read n) [1..]
                 y = show $ zip num fibs2
             in putStrLn y}

{--
main4 = do
        putStrLn "Enter number:"
        n <- getLine
        let nn = read n
            x = take nn [1..]
            y = take nn fibs2
        in return $ z x y
        where z = zipWith (,)
              z2 = nn
              z3 = zip
              z4 = x
--}

main3 = do
        putStrLn "Enter number:"
        n <- getLine
        let nn = read n
            x = take nn [1..]
            y = take nn fibs2
         in return $ z x y      -- Backspace aligning "in" below "let" wont work!
        where z = zipWith (,)
              z2 = z3
              z3 = zip
              z4 = z3
              --z5 = x  -- x is out of scope

ff3 = let x = take (read "5") [1..]
          y = take 5 fibs2
      in z x y
      where z = zipWith (,)

main5 = do
        putStrLn "Enter number:";
        n <- getLine;
        let nn = read n
            x = take nn [1..]
            y = take nn fibs2
         in return $ z x y;      -- Backspace aligning "in" below "let" wont work, even w/ ";" delims!
        where z = zipWith (,)
              z2 = z3
              z3 = zip
              z4 = z3

main6 = do{
        putStrLn "Enter number:";
        n <- getLine;
        let nn = read n
            x = take nn [1..]
            y = take nn fibs2
        in return $ z x y}      -- Aligning "in" below "let" works when using both "{}" AND ";"
        where z = zipWith (,)
              z2 = z3
              z3 = zip
              z4 = z3

{--
Rule 1: With "do{}", ";" is must, except last statement where ";" is optional.
Rule 2: With "do" without "{}", ";" may or may not be used in any statement (including last
        statement), independent of any other statement in the do-block.
--}

{--
Below ff6 does not work because the "where" cannot see x and y.
In other words, variables x and y are out of scope at the "where".
ff6 = let x = take (read "5") [1..]
          y = take 5 fibs2
      in z
      where z = zipWith (,) x y
--}

{--
Found that main1, main2, ff4, ff5 below don't work.
Found that "where" must be outside the "do" block.
So, there can be only one "where", which is outside the "do" block,
and it cannot refer to any variable from inside, otherwise will get "variable out of scope" error.

main1 = do
          putStrLn "Hello World! What is your name?";
          name <- getLine;
          putStrLn $ "Hi " ++ name ++ " :) Please enter a number:";
          n <- getLine;
          print $ show $ take (read n) [1..];
          print . show . take (read n) $ [1..];
          do{
          putStrLn "s"}
          where num = take (read n) [1..]
                y = zip num fibs2


main2 = do
          putStrLn "Hello World! What is your name?";
          name <- getLine;
          putStrLn $ "Hi " ++ name ++ " :) Please enter a number:";
          n <- getLine;
          print $ show $ take (read n) [1..];
          print . show . take (read n) $ [1..];
          putStrLn "s";
          where num = take (read n) [1..]
                y = zip num fibs2


ff4 = do
     {name <- getLine;
      putStrLn "y";}
       where y = name
             z = "t"


ff5 = do
        name <- getLine;
        putStrLn "y"
         where y = name
               z = "t"

--}

ff = do
     {name <- getLine;
      putStrLn "y";}
       where y = "s"
             z = "t"

ff1 = do
     {putStrLn "y"}
       where y = "s"
             z = "t"

ff2 = do
      {let y = "s"
           z = "t" in putStrLn "y";}



--kpMap :: Monad f => (a -> b) -> f a -> f b
--kpFmap = \f x -> x >>= \x' -> return (f x')
{--
kpMap f x = do
            x' <- x
            return (f x')
--}


--kpAp = \f x -> x >>= \x' -> f >>= \f' -> return (f' x')
{--
kpAp f x = do
           x' <- x
           f' <- f
           return (f' x')
--}

{--
kpJoin x = do {x' <- x; x}
kpJoin = \x -> x >>= id
kpJoin = \x -> x >>= \x' -> x'
--}

--count :: a -> Int
--count _ = 0


fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 = map fib [0..]

fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

data Crispy a = Snap a [a] a
              | Crackle [[Crispy a]]
              | Pop Integer deriving (Eq,Show)

instance Functor Crispy where
  fmap f (Snap x xs y) = Snap (f x) (map f xs) (f y)
  fmap f (Crackle css) = Crackle $ (map . map . fmap) f css
  fmap _ (Pop n) = Pop n 

instance Foldable Crispy where
  foldMap f (Snap x xs y) = (f x) `mappend` (foldMap f xs) `mappend` (f y)
  foldMap f (Crackle css) = (foldMap . foldMap . foldMap) f css
  foldMap f (Pop n) = mempty


data Rose a = a :> [Rose a] deriving (Eq,Show)

instance Functor Rose where
  fmap f (x :> xs) = (f x) :> ((fmap . fmap) f xs)

class SumRes r where
    sumOf :: Integer -> r

instance SumRes Integer where
    sumOf = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    sumOf x = sumOf . (x +) . toInteger

class CountRes r where
    count :: Integer -> r

--instance CountRes Integer where
--    count = 0

--instance CountRes r => CountRes (a -> r) where
--    count _ = count . (+1)
