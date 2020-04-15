module TypeClassTest where

{--
class TypeClassTest a where
yesno :: a -> Bool
--}

--data T1 = T1 String

--data T2 = T2 Int

class MyClassT a where
invoke1 :: a -> Int
invoke1 = \_ -> 0

{--
instance MyClassT T1 where
invoke (T1 s) = read s

instance MyClassT T2 where
invoke (T2 n) = n

--}