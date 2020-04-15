module T2 where

import TypeClassTest

data T2 = T2 Int

instance MyClassT T2 where
invoke1 (T2 n) = n