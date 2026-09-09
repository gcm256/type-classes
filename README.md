# type-classes

<pre>
fmap = flip (>>=) . (.) return

Proof:

fmap = \f xs -> xs >>= (\x -> return (f x))           -- Defn of >>=
     = \f xs -> xs >>= (\x -> (return . f) x)         -- Since: f (g x) = (f . g) x
     = \f xs -> xs >>= (return . f)                   -- Since, by eta-reduction: \x -> f x = f
     = \f xs -> (>>=) xs (return . f)                 -- Writing >>= as prefix function from infix notation
     = \f xs -> flip (>>=) (return . f) xs            -- Defn of flip
     = \f -> flip (>>=) (return . f)                  -- Eta-reduction
     = \f -> flip (>>=) ((.) return f)                -- Writing (.) as prefix function
     = \f -> (flip (>>=)) (((.) return) f)            -- Rewrite with redundant parens, as f (g x)
     = \f -> ((flip (>>=)) . ((.) return)) f          -- Rewrite f (g x) as (f . g) x
     = ((flip (>>=)) . ((.) return))                  -- Eta-reduction
     = flip (>>=) . (.) return                        -- Remove redundant parens
</pre>

## Relationship between Field, Ring, Abelian Group, Group, Monoid, Semi-group

Field $(+, \times)$ ⟹ Ring $(+, \times)$ ⟹ Abelian Group $(+)$ ⟹ Group $(+)$ ⟹ Monoid $(+)$ ⟹ Semigroup $(+)$

## Summary of Logic

### \[Logical foundation\] What kind of reasoning?
* Classical Logic (Propositional Logic, First Order Logic (FOL), HOL)
* Intuitionistic Logic (Constructive Logic (Brouwer))

### \[Structural discipline\] How may assumptions be used?
* Structural(Ordinary)
* Substructural Logic(Linear Logic)

### \[Expressive logical extensions\] What extra concepts/operators?
* Modal Logic
* Temporal Logic (LTL, CTL, CTL*)

### \[Semantics\] What gives meaning to the logic?
* Heyting Algebras
* Boolean Algebras
* Kripke models

### \[Proof system\] How do we formally derive proofs?
* Hilbert
* Natural Deduction
* Sequent Calculus (Gentzen)

