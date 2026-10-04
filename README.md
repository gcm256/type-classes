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

### Taxonomy

| Dimension	                         | Question                                           |	Examples |
| ----                                  | ----                                               | ----     |
| 1. Logical foundation / principles	| What notion of validity/reasoning?	             | Classical, Intuitionistic, Minimal |
| 2. Structural discipline / rules	     | How may assumptions be used?	                  | Structural, Linear, Affine, Relevant |
| 3. Logical extensions	               | What additional operators/concepts?	             | Modal, Temporal, etc. |
| 4. Logical order / expressiveness	| What can propositions quantify over?	             | Propositional → First-order → Second-order → Higher-order |
| 5. Dependent type expressiveness	     | Can types/propositions depend on terms, types etc? | Dependent Type Theory, Simple, polymorphic, higher-kinded |
| 6. Semantics	                         | What gives the system meaning?	                  | Boolean algebras, Heyting algebras, Kripke models, categorical semantics, etc. |
| 7. Proof calculus	                    | How are proofs derived and presented?	             | Hilbert, Natural Deduction, Sequent Calculus |
| 8. Computational/proof-term calculus	| What computational objects correspond to proofs?   | Combinatory logic, λ-calculus, dependent λ-calculus |

## Specification of a Logic

Specification of a logic, is the quintuple with the following elements: (1, 2, 4, 5, 7)

And then, Specification ⟶ {type/proof calculi satisfying it}

where the result can have cardinality: 0, 1, or many.


### \[Logical foundation\] What kind of reasoning?
* Classical Logic (Propositional Logic, First Order Logic (FOL), HOL)
* Intuitionistic Logic (Constructive Logic (Brouwer))

### \[Structural discipline\] How may assumptions be used?
* Structural (Ordinary Logic)
* Substructural Logic
  * Linear Logic
  * Affine Logic
  * Relevant Logic

### \[Expressive logical extensions\] What extra concepts/operators?
* Modal Logic
* Temporal Logic (LTL, CTL, CTL*)

### \[Logic Order/Expressiveness/Strength + Type Dependency/Expressiveness/Structure\] What can propositions quantify over? + What can types depend upon?
* Propositional + Simple(Non-dependent) => Simply-typed λ-calculus (STLC)
* Propositional + Dependent => Not-applicable
* First Order + Non-dependent => ?
* First Order + Dependent => Π/Σ type systems
* Second-Order + Polymorphic(Non-dependent) => System F
* Second Order + Dependent => Dependent second-order type systems
* Higher-Order + Higher-Kinded(Non-dependent-) => Fω / Higher-order λ-calculi
* Higher-Order + Dependent => CoC (Calculus of Constructions)
* Higher-Order + Dependent Inductive Types => CIC (Calculus of Inductive Constructions)
  * ie CoC + Inductive Types => CIC

### \[Semantics\] What gives meaning to the logic?
* Heyting Algebras
* Boolean Algebras
* Kripke models
* Domain Semantics
* Categorical Semantics

### \[Proof system\] How do we formally derive proofs?
* Hilbert ↔ Typed Combinatory Logic
* Natural Deduction ↔ Typed λ-calculus
* Sequent Calculus (Gentzen) ↔ Cut-elimination / computational calculi

