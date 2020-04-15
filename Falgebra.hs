{-# LANGUAGE DeriveFunctor #-}
data ExprF r = Const Int
             | Add r r
             | Mul r r
    deriving Functor

newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

alg :: ExprF Int -> Int
alg (Const i)   = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

eval :: Fix ExprF -> Int
eval = cata alg

testExpr = In $
             (In $ (In $ Const 2) `Add` (In $ Const 3)) `Mul`
             (In $ Const 4)

main = print $ eval $ testExpr