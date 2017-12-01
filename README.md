# type-classes

fmap = flip (>>=) . (.) return

Proof:

fmap = \f xs -> xs >>= (\x -> return (f x))           -- Defn of >>=
     = \f xs -> xs >>= (\x -> (return . f) x)         -- Since: f (g x) = (f . g ) x
     = \f xs -> xs >>= (return . f)                   -- Since, by eta-reduction: \x -> f x = f
     = \f xs -> (>>=) xs (return . f)                 -- Writing >>= as prefix function from infix notation
     = \f xs -> flip (>>=) (return . f) xs            -- Defn of flip
     = \f -> flip (>>=) (return . f)                  -- Eta-reduction
     = \f -> flip (>>=) ((.) return f)                -- Writing (.) as prefix function
     = \f -> (flip (>>=)) (((.) return) f)            -- Rewrite with redundant parens, as f (g x)
     = \f -> ((flip (>>=)) . ((.) return )) f         -- Rewrite f (g x) as (f . g) x
     = ((flip (>>=)) . ((.) return ))                 -- Eta-reduction
     = flip (>>=) . (.) return                        -- Remove redundant parens 
