module T1 where

import TypeClassTest

data T1 = T1 String

instance MyClassT T1 where
invoke1 (T1 s) = read s

