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

See also: [Curry-Howard Mapping](curry-howard-mapping)

| Level | What is allowed to depend on what? | Typical example |
|---|---|---|
| L0️⃣: Propositional Logic | No quantification over individuals or propositions/types; propositions are atomic units | STLC |
| L1️⃣: First-order (Predicate) Logic | Quantification over individuals/terms (Dependent typing) | First-Order Dependent type-systems eg λP / LF, λP extended, Martin-Löf Type Theory (MLTT) without Universes |
| L1️⃣: Second-order-propositional Logic | Quantification over propositions/types | System F |
| L2️⃣: Second-order-predicate Logic | Quantification over individuals (Dependent typing) and over predicates/relations | Richer/higher-order Dependent type-systems eg Second-Order (Polymorphic) Dependent λ-calculus (λP2 / PRED2) |
| L2️⃣: Higher-order-propositional Logic | Quantification over higher-order predicates/functions/types (eg over predicates of predicates, etc.) using higher-order quantification/type operators | Fω / higher-order type systems |
| L3️⃣: Higher-order-predicate Logic | Types depend on terms (Dependent typing) and on higher-order/type-level abstraction is available | Calculus of Constructions (CoC) |
| L4️⃣: Full Higher-order Logic with Inductive Definitions | CoC features plus inductive types/constructions | CIC |
| L5️⃣: Higher-order Mathematics (Logic + Arithmetic) | Dependent typing + native data-structures for logic and math + Universes | MLTT |

> [!IMPORTANT]
> :sparkle: **Predicate Logic** (of any Order > 0) requires **Dependent-typing**. Propositional Logic (of any Order) doesn't require Dependent-typing.
>
> :high_brightness: Logics at the same Level are on different axes, hence not comparable. Eg L1️⃣: First-order (Predicate) Logic and L1️⃣: Second-order-propositional Logic are
> not comparable since the former is along the x-axis and the latter along the y-axis of the Lambda Cube. Similarly, λP2 (λP+λ2) and Fω (λ2+λ⍹) are both L2️⃣.
> 
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
| Non-dependent | No dependence between Types and Terms | **STLC** (**$\lambda \to$**) | None(Baseline) | Terms can depend on terms only $(*, *)$ |
| Polymorphic | Quantification over types. Terms can be parametrized by types. | **System F** (**$\lambda 2$**) | Type abstraction (∀) | Terms can depend on types/terms $(\Box/\*, \*)$ |
| Higher-kinded | Types/type operators can be higher-order. Types can be parametrized by types. | **Fω** (**$\lambda 2 + \lambda\underline{\omega}$**) | Type Constructors aka type-level functions | Types can depend on types $(\Box, \Box)$. (In addition to **System F** capability $(\Box/\*, \*)$ ) |
| Dependent | Types can be parametrized by terms | Dependent λ-calculus (**λP** / LF) | Dependent types (Π-types) | Types/terms can depend on terms $(\*, \Box/\*)$ |
| Dependent + Polymorphic | Types can be parametrized by terms. Terms can be parametrized by types. | Second-Order (Polymorphic) Dependent λ-calculus (**λP2** / **PRED2**)[^15] | Dependent types (Π-types) & Type abstraction (∀) | Types can depend on terms $(\*, \Box)$. (In addition to **System F** capability $(\Box/\*, \*)$ ) |
| Higher-order + dependent + inductive | Term-dependent types with native data structures and universes | Martin-Löf Type Theory (MLTT) | In addition to Dependent types (Π-types): Dependent pairs (Σ-types), Identity-types, Inductive trees (W-types) + Predicative Universes | Types can depend on terms + Native mathematical induction. Does not fit the Barendregt $(\text{sort}_1, \text{sort}_2)$ Lambda Cube notation. |
| Higher-order + dependent | Polymorphic + Higher-kinded + Dependent | Calculus of Constructions (CoC) | Full Lambda Cube integration | Terms/Types can depend on Terms/Types $(*/\Box, */\Box)$ |
| Higher-order + dependent + inductive | CoC extended with inductive data types and universes | Calculus of Inductive Constructions (CIC) | Inductive types + Predicative Universes (replacing the $\Box$ of CoC) + Impredicative `Prop` (replacing the $\ast$ of CoC) | CoC + Native data structures + Consistent mathematical proofs |

> [!IMPORTANT]
> Standard MLTT does not cleanly fit the Barendregt $(\text{sort}_1, \text{sort}_2)$ Lambda Cube notation.
> The Lambda Cube is strictly bounded by two specific sorts: $\ast$ (the universe of terms/types) and $\Box$ (the universe of kinds).
> MLTT breaks this geometry because it uses an **infinite, cumulative hierarchy of universes** (𝒰₀, 𝒰₁, 𝒰₂, …).
> Because it lacks a single, absolute top sort like $\Box$ and rejects global impredicativity, it cannot be modeled as a simple corner of the standard 3-axis cube.
>
> **MLTT Universes vs. CIC Predicative Universes:**
> 
> Both MLTT and the Calculus of Inductive Constructions (CIC) use a matching **infinite, cumulative, stratified** hierarchy of Predicative Universes (𝒰₀, 𝒰₁, 𝒰₂, …) (often called **`Type0`** or `Set`,
>  **`Type1`**,**`Type2`**, etc. in programming syntax) to prevent Russel-style paradoxes.
> 
> **The main difference:** CIC adds one extra, special universe that MLTT refuses to include: an **impredicative** universe called `Prop` (used for mathematical propositions).
> MLTT disallows such "Impredicative Polymorphism" and remains strictly predicative throughout its entire universe structure.
>
> When looking at the above table's classification system (which follows Barendregt's Lambda Cube),
> "Higher-order" specifically refers to systems like System F and Fω that have **impredicative, parametric polymorphism** (∀ X. T).
> 
> - **System F / CoC / CIC:** You can write a function that works for absolutely every type that will ever exist, including the type of the function itself (impredicativity).
>
> - **MLTT:** To avoid paradoxes while including inductive types, MLTT rejects this kind of global impredicativity. Instead, it uses a stratified hierarchy of **Universes**
>   (𝒰₀, 𝒰₁, …). A type in universe 𝒰₀ cannot quantify over 𝒰₀ itself; it must step up to 𝒰₁.
> 


##  Curry–Howard Mapping 

| Logic | Type-theoretic / λ-calculus counterpart |
|---|---|
| Intuitionistic propositional logic (aka Zeroth-order Propositional Logic aka Zeroth-order Predicate Logic) | STLC |
| ~~First-Order intuitionistic propositional logic~~ | No such logic. "Propositional" means there are no quantifiers over **individuals**, whereas "first-order" means there are. |
| Second-order intuitionistic propositional logic (Polymorphic) | System F |
| Higher-order intuitionistic propositional logic (Higher-kinded) | Fω |
| First-Order Predicate Logic (Minimal Fragment) (only $\forall, \to$) | First-Order Dependent λ-calculus (**λP** / LF)[^15]. <br> • **$\Pi$-types** only = **($\forall, \to$)** |
| Full First-Order Intuitionistic Logic (All connectives, no native arithmetic/induction) | **λP extended** <br> • **$\Pi$-types** (already in λP), <br> • **$\Sigma$-types** = ($\land$, $\exists$), <br> • **Sum-types**(**$+$**) = ($\lor$), <br> • **$\mathbf{0}$** = ($\bot$ / **`False`**), <br> • **$\mathbf{1}$** = ($\top$ / **`True`**) |
| Full First-Order Intuitionistic Logic with Native Induction and Arithmetic (∀, ∃) | Martin-Löf Type Theory (MLTT) **without Universes** **$\dagger$** |
| Second-Order Predicate Logic | Second-Order (Polymorphic) Dependent λ-calculus (**λP2** / **PRED2**)[^15] |
| Higher-Order Predicate Logic (∀ over types and predicates) | Calculus of Constructions (CoC) |
| Full Higher-Order Intuitionistic Logic with Inductive Definitions | Calculus of Inductive Constructions (CIC) |
| Higher-Order Intuitionistic (Constructive) Mathematics (Heyting Arithmetic) with Universes | Standard Martin-Löf Type Theory (MLTT) |

> [!IMPORTANT]
> **$\dagger$** 
> Restricting Martin-Löf Type Theory (MLTT) by removing all type universes does not introduce impredicativity. The resulting system remains strictly predicative.

## Decidability / Computability

### Strong-normalization (SN)

For an arbitrary λ-term $M$, in an arbitrary Type-System, let:

$\text{SN}(M) \equiv \text{ Every reduction sequence starting from } M \text{ terminates.}$

The **General** problem **"Is $\text{SN}(M)$ True?"** is **Undecidable**.

> [!CAUTION]
> Are there particular type-systems for which **SN** is **Decidable**?
> 
> Answer: Yes. Strong Normalization (SN) is decidable for all systems within the Lambda Cube, as well as standard Dependent Type Theories (like MLTT and CIC), and Pure Type Systems.
> 
> Reason:
> - For all Type Systems within the Lambda Cube, **The Strong Normalization Theorem** have been structurally and mathematically proven.
> - For Pure Type Systems & Dependent Type Systems, modern constructive type theories enforce strict structural termination checks (like ensuring recursive calls are
>   only made on strictly smaller sub-components), the system ensures that both type-checking and strong normalization remain fully decidable.
>   
> There are of course other Type-Systems whose SN status are **Undecidable**.
> - eg: SN status of Intersection Type Systems as a whole is Undecidable. (Even if the SN status of a given a well-typed SN term in this system is decidable.)

* **Strong-normalization (SN)**: All paths lead to termination.
* **Weak-normalization (WN)**: At least one path leads to termination.
* **Non-WN** / **Non-termination**: No path leads to termination.

> [!NOTE]
> * Pure type-systems (eg STLC, System F, Fω, λP / LF, CoC, MLTT, CIC etc) are **Strongly-normalizing**.
>   * Strong-normalization $\implies$ (**ONE-WAY-ONLY**) Weak-normalization
>     * So, Strong-normalization $\subset$ (**PROPER**) Weak-normalization
>   * Weak-normalization $\implies$ (**ONE-WAY-ONLY**) **Only total** (No partial) functions allowed.
>   * Hence, by transitivity, Strong-normalization $\implies$ (**ONE-WAY-ONLY**) **Only total** (No partial) functions allowed.
>   
> * Type-System of Haskell is **System FC** (Fω + Coercions)
>   * Coercions = First-class type equality proofs
>   *  **System FC** allows **partial functions**. Hence, Haskell the language, is non-WN, even if it has terms that are SN (terms that always terminate) or WN (terms that terminate only by lazy-evaluation eg `head [1, infiniteLoop]`)
>   *  Hence a Haskell program may not terminate.
>   *  But: Since Coq uses CIC, a Coq program will always terminate. And Coq only allows total functions.

### Type-related Computations

| Type-System	| Type-Checking	| Definitional-equality[^16]	| Type-Inference[^17]  |
| ----         | ----              | ----                        | -----                |
| STLC	     | ✅ Decidable	     | ✅ Decidable	               | ✅ Decidable         |
| System F	| ✅ Decidable*	| ✅ Decidable	               | ❌ Undecidable       |
| Fω	          | ✅ Decidable*	| ✅ Decidable	               | ❌ Undecidable       |
| λP / LF      | ✅ Decidable*	| ✅ Decidable	               | ❌ Undecidable       |
| CoC	     | ✅ Decidable*	| ✅ Decidable	               | ❌ Undecidable       |
| MLTT	     | ✅ Decidable**	| ✅ Decidable**	          | ❌ Undecidable       |
| CIC	     | ✅ Decidable**	| ✅ Decidable**	          | ❌ Undecidable       |
| GHC Haskell	| ❌ Undecidable	| ❌ Undecidable	          | ❌ Undecidable       |


\* Assuming the explicitly typed formulation.

\*\* For standard, well-behaved versions with a decidable conversion procedure and strictly positive inductives/universe rules.

## Appendix and References[^1][^2][^3][^4][^5][^6][^7][^8][^9][^10][^11][^12][^13][^14]

[^1]: https://en.wikipedia.org/wiki/Lambda_cube
[^2]: https://en.wikipedia.org/wiki/Intuitionistic_type_theory#Martin-Löf_type_theories
[^3]: https://en.wikipedia.org/wiki/Template:Foundations-footer
[^4]: https://en.wikipedia.org/wiki/Template:Non-classical_logic
[^5]: https://en.wikipedia.org/wiki/Curry-Howard_correspondence
[^6]: https://archive-pml.github.io/martin-lof/pdfs/Bibliopolis-Book-retypeset-1984.pdf
[^7]: [The collected works of Per Martin-Löf](https://archive-pml.github.io/)
[^8]: https://en.wikipedia.org/wiki/Template:Logic
[^9]: https://en.wikipedia.org/wiki/Template:Mathematical_logic
[^10]: https://wiki.haskell.org/index.php?title=Typeclassopedia
[^11]: https://en.wikipedia.org/wiki/Template:Formal_semantics
[^12]: https://en.wikipedia.org/wiki/Template:Programming_paradigms_navbox
[^13]: https://en.wikipedia.org/wiki/Template:Design_patterns
[^14]: :play_or_pause_button: [Chris Casinghino - Making Dependent Types Practical](https://youtu.be/_2jrmgO_Gq0)
[^15]: https://en.wikipedia.org/wiki/Dependent_type#First_order_dependent_type_theory
[^16]: Definitional-equality (aka Type-conversion: Are two terms definitionally equal?)
[^17]: Type-Inference (aka Type-inhabitation)
