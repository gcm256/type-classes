Wrt: "Term → Type is an Order-2 Kind", Term -> Type is First-order, no?

Wrt: "What actually separates Propositional Logic from Predicate Logic is whether terms/individuals are allowed to parameterize types at all.", But Propositional means 0-arity, correct?

--

## 1. Term → Type is First-Order Predicate Logic, No?
Yes, Term → Type is the core structural component of First-Order Predicate Logic.
The confusion here comes from a subtle distinction between the order of the Kind signature itself and the order of the logical system it builds.

* The Logical System: In First-Order Predicate Logic ($\lambda P$ / LF), the framework introduces the Kind signature Term → Type so you can write a standard property like $\text{Prime} : \text{Term} \to \text{Type}$. Crucially, the logic only allows you to quantify over the Order-1 base terms ($\forall x : \text{Term}$). Because the predicate variable itself is never quantified over, the system is First-Order.
* The Variable Level: If you elevate the system to Second-Order Predicate Logic ($\lambda P2$), you are now allowed to quantify over a variable $P$ whose signature is Term → Type ($\forall P : \text{Term} \to \text{Type}$). Because you are abstracting over an Order-2 Kind signature, it constitutes Second-Order quantification.

Summary for your notes: Term → Type belongs to the Predicate row. If you quantify over its inputs (Term), you are in First-Order Logic. If you quantify over the predicate itself as a variable, you are in Second-Order Logic.
------------------------------
## 2. Does "Propositional" Mean 0-Arity?
Yes, Propositional means 0-arity at the outer formula level.
Your definition is completely correct. A fully constructed proposition must have an arity of 0 (Kind: Type) because it has no open parameter slots waiting for an input.
The defining separation between the rows on your grid comes down to what is allowed to exist inside the internal variable abstractions:

* The Propositional Row (0-Arity Framework): The system completely lacks a Term layer. No matter how deep you look into the quantifiers or internal variables, you will never see a Term input parameter. Every bound variable is either a plain proposition (Type) or a function of propositions (Type → Type).
* The Predicate Row ($n$-Arity Framework): The system introduces a Term layer. This allows you to construct internal variable signatures that accept terms as parameter inputs (like Term → Type), meaning types can now depend natively on terms (dependent typing).

## The Locked-In Matrix View
To make your taxonomy completely bulletproof, the two axes measure exactly this:

   1. Vertical Axis (Language Type): Measures whether Term inputs are completely banned from internal variable signatures (Propositional) or actively permitted (Predicate), which introduces dependent types.
   2. Horizontal Axis (Order): Measures the structural height of the variables being targeted by the quantifiers ($\forall, \exists$) inside the 0-arity formula.

Now that the relationship between 0-arity propositions, internal variable signatures, and logical orders is fully aligned, would you like to:

* Generate the complete, final Markdown table using this exact structural nomenclature?
* Draft formal definitions for the rows and columns to lock directly into your notebook?

==

Yes Generate the complete, final Markdown table using the below template. Even though I am not fully convinced that what you said just now is really the correct formulation.

| $\underbrace{\text{Arity}}_\downarrow \mid \text{Order} \to$ | Zeroth-order | First-order | Second-order | Higher-order |
| ----                                                         | ----         | ----        | ----         | ----         |
| Propositional ($0$-ary Predicate)                            |              |             |              |              |
| Predicate ($n$-ary Predicate, $n \ge 1$)                     |              |             |              |              |

--

I completely respect your skepticism. The intersection of type theory, the Lambda Cube, and classical logical nomenclature is notoriously one of the most ambiguous areas of formal systems because logicians and computer scientists shifted definitions as frameworks evolved.
To give you the most accurate tool for your notebook, here is the complete, final Markdown table mapping the systems based exactly on what the quantifiers natively bind inside a fully saturated, 0-arity proposition, aligned alongside their canonical Curry-Howard isomorphism pairs (the Lambda Cube vertices).

| $\underbrace{\text{Arity}}_\downarrow \mid \text{Order} \to$ | Zeroth-order | First-order | Second-order | Higher-order |
|---|---|---|---|---|
| Propositional ($0$-ary Predicate) | $\lambda\to$ (Simply Typed $\lambda$-Calculus) • No quantifiers allowed • Only static propositional constants (e.g., $P \to Q$). | Not Applicable (Requires a Term layer to exist in the syntax for first-order quantification). | $\lambda 2$ (System F / SOPL) • Quantifies over Type • Binds 0-ary propositional variables: $\forall P : \text{Type}$. | $\lambda\omega$ (System F$\omega$ / HOPL) • Quantifies over Type \to Type • Binds logical connectives/boolean functions: $\forall F : \text{Type} \to \text{Type}$. |
| Predicate ($n$-ary Predicate, $n \ge 1$) | Not Applicable (Requires term variables to saturate the internal $n$-ary predicate functions). | $\lambda P$ (Logical Framework LF) • Quantifies over Term • Binds individual objects: $\forall x : \text{Term}$. • Predicates (Term \to Type) are static constants. | $\lambda P2$ (Second-Order Predicate) • Quantifies over Term \to Type • Binds unary properties / sets of individuals: $\forall P : \text{Term} \to \text{Type}$. | $\lambda P\omega$ / CoC (Calculus of Constructions) • Quantifies over Kinds • Binds meta-predicates / properties of predicates: $\forall \mathcal{X} : (\text{Term} \to \text{Type}) \to \text{Type}$. |

If you want to debug or test the boundary lines of this formulation further, let me know:

* Do you want to see a syntax breakdown of how a specific formula (like Leibniz Equality) is evaluated by a type checker?
* Would you like to map out how impredicativity behaves differently in the Second-Order ($\lambda 2$) vs. Higher-Order ($\lambda\omega$) cells?



https://share.google/aimode/vkz72PWBbDEb2HWLx
