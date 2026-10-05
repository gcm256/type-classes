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

**Specification** of a logic, is the **Quintuple** with the following elements: **(1, 2, 4, 5, 7)**

And then, **Specification ⟶ {type/proof calculi satisfying it}**

where the result can have cardinality: 0, 1, or many.

## Dimensions of a Logic

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
* First Order + Non-dependent => No standard direct Curry–Howard λ-calculus counterpart
* First Order + Dependent => Π/Σ type systems
* Second-Order + Polymorphic(Non-dependent) => System F
* Second Order + Dependent => Dependent second-order type systems
* Higher-Order + Higher-Kinded(Non-dependent-) => Fω / Higher-order λ-calculi
* Higher-Order + Dependent => CoC (Calculus of Constructions)
* CoC + Inductive Types/Constructions => CIC (Calculus of Inductive Constructions)
  * ie Higher-Order + Dependent + Inductive Types => CIC

> [!IMPORTANT]
> * The computation-calculi named on the RHS are **ONLY** for the LHS setting with `Foundation = Intuitionistic` AND `Proof-System = ND`.
> * For all the other settings, eg Second-order + Dependent + (Classical + ND, or, Intuitionistic + Hilbert, or, Classical + Hilbert), the computation-calculi potentially exist, but don't have names and do not necessarily uniquely determine a type system.
> 
> This is the crucial distinction:
> 
> `Foundation + Order + Dependency + Proof calculus` specifies a design point, but does not necessarily uniquely determine a type system.
>
> For example: `(Intuitionistic, Second-order, Non-dependent, ND)` has the canonical representative **System F**.
>
> But: `(Classical, Second-order, Non-dependent, ND)` has multiple possible classical λ-calculus formulations.

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

## Terminology: Individual, Term, Proposition, Predicate

| Concept | Example | Meaning |
|---|---|---|
| Individual | `Alice`, `42` | An object in the domain |
| Term | `x`, `42`, `f(x)` | Syntactic expression denoting an individual |
| Function | `f` | Maps individuals to individuals |
| Predicate | `Human`, `Even`, `Loves` | Property/relation of individuals |
| Proposition | `Human(Alice)`, `2+2=4` | Something that can be true or false |
| Propositional variable | `P`, `Q` | Variable ranging over propositions |
| Predicate variable | `P`, `R` | Variable ranging over predicates/relations |
| Type | `Nat`, `Bool`, `A` | Class/category of terms |
| Type variable | `X`, `Y` | Variable ranging over types |
| Dependent type | `Vec(A,n)` | A type whose definition depends on a term |

```
What can the system quantify over?
│
├── Propositions
│     └── System F-style second-order propositional logic
│
├── Individuals
│     └── First-order logic
│
├── Predicates over individuals
│     └── Second-order predicate logic
│
├── Higher-order predicates/functions
│     └── Higher-order logic
│
└── Multiple levels simultaneously
      └── Higher-order dependent type theory / CoC
```

## Logical-Order & Type-Dependency

| Level | What is allowed to depend on what? | Typical example |
|---|---|---|
| Propositional Logic | No quantification over individuals or propositions/types; propositions are atomic units | STLC |
| First-order Logic | Quantification over individuals/terms | First-order intuitionistic logic |
| Second-order-propositional Logic | Quantification over propositions/types | System F |
| Second-order-predicate Logic | Quantification over individuals and predicates/relations | Richer dependent/higher-order type systems |
| Higher-order Logic | Quantification over higher-order predicates/functions/types (eg over predicates of predicates, etc.) using higher-order quantification/type operators | Fω / higher-order type systems |
| Dependent Types | Types/propositions may depend on terms | Dependent λ-calculus / MLTT |
| Higher-order Dependent Types | Types depend on terms and higher-order/type-level abstraction is available | Calculus of Constructions (CoC) |
| Higher-order Dependent Inductive Types | Above plus inductive types/constructions | CIC |

> [!IMPORTANT]
> Second-order predicate logic and second-order propositional logic are different logical formalisms, not simply successive levels of one hierarchy. However, a dependent type theory
> capable of representing second-order predicate logic has capabilities that System F lacks, particularly term-dependent types and quantification over predicates of individuals.
> The type-theoretic counterpart of 2nd-Order predicate logic can be richer than that of 2nd-order propositional logic even though the two source logics aren't naturally ordered by
> “second-order-ness.”

| System	                                     | Individuals	| Predicates over individuals	| Quantification over propositions/types | Term-dependent types |
| ----                                         | ----            | ----                        | ----                                   | ----                 |
| Propositional logic	                      | ❌	          | ❌	                         | ❌	                                    | ❌ |
| Second-order propositional logic / System F  | ❌	          | ❌	                         | ✅	                                    | ❌ |
| First-order logic	                           | ✅	          | Fixed predicate symbols	| ❌	                                    | Conceptually representable via dependent types |
| Second-order predicate logic	            | ✅	          | ✅	                         | Not as a separate quantifier	      | Conceptually representable via dependent types |
| Rich dependent type theory	                 | ✅	          | ✅	                         | ✅	                                    | ✅ |

## Logical-Order & Proof Calculus

| Logical order / strength | Hilbert | Natural Deduction | Sequent Calculus |
|---|---|---|---|
| Propositional | Propositional Hilbert calculus ↔ typed combinatory logic | Intuitionistic ND ↔ STLC | Propositional sequent calculus |
| First-order | First-order Hilbert calculus | First-order natural deduction | First-order sequent calculus |
| Second-order | Second-order Hilbert calculus | Second-order **propositional** intuitionistic ND ↔ System F | Second-order sequent calculus |
| Higher-order | Higher-order Hilbert systems | Higher-order ND ↔ higher-order λ-calculi / Fω-related systems | Higher-order sequent calculus |

> [!IMPORTANT]
> Second-order logic can be Second-order-**propositional** logic (quantifies over propositions/types) or Second-order-**predicate** logic (quantifies over individuals + predicates).

## Type-Dependency & Proof Calculus

| Type/proposition dependency | Meaning | Typical calculus | Feature added | Main capability |
|---|---|---|---|---|
| Non-dependent | Types do not depend on terms | STLC | - | Terms depend on terms |
| Polymorphic | Quantification over types | System F | Polymorphism | Terms can depend on types |
| Higher-kinded | Types/type operators can be higher-order | Fω | Type Constructors aka type-level functions | Types can depend on types |
| Dependent | Types can depend on terms | Dependent λ-calculus / Martin-Löf Type Theory | Dependent types | Types can depend on terms and types |
| Higher-order + dependent | Higher-order type abstraction and term-dependent types | Calculus of Constructions (CoC) | Dependent types | Types can depend on terms and types |
| Higher-order + dependent + inductive | Above + inductive types/constructions | Calculus of Inductive Constructions (CIC) | inductive types + universes | CoC + datatypes/proofs |

##  Curry–Howard Mapping 

| Logic | Type-theoretic / λ-calculus counterpart |
|---|---|
| Intuitionistic propositional logic | STLC |
| Second-order intuitionistic propositional logic | System F |
| Higher-order polymorphic type theory | Fω |
| First-order intuitionistic logic | Representable in dependent type theory |
| Dependent intuitionistic type theory | Dependent λ-calculus / Martin-Löf Type Theory |
| Higher-order dependent intuitionistic type theory | Calculus of Constructions (CoC) |
| Dependent type theory + inductive constructions | Calculus of Inductive Constructions (CIC) |

