# Factorization and Primality Across Number Systems, Algebraic Structures, Rings, and Fields

| Structure / System | Classification | Factorization & Primality Behavior |
| :--- | :--- | :--- |
| **Positional Bases** (e.g., Binary, Decimal) | Notation / Representation | **Invariant:** Changes digit representation, but leaves the underlying arithmetic and primes untouched. |
| **Standard Integers ($ℤ$)** | Unique Factorization Domain (UFD) | Standard unique prime factorization (Fundamental Theorem of Arithmetic). |
| **Polynomial Rings over Fields ($F[x]$)** | Euclidean Domain (UFD) | Division algorithm works smoothly (PID); unique factorization (into irreducibles) holds (analogously to $ℤ$); primality translates to **irreducible polynomials**.|
| **Polynomial Rings over Integers ($ℤ[x]$)** | Non-PID UFD | Division fails (not a PID), but **Gauss's Lemma** ensures unique factorization (into irreducibles) holds (again analogously to $ℤ$); so primality still translates to **irreducible polynomials**. |
| **Multivariate Polynomials ($F[x_1, \dots, x_n]$)** | Noetherian Ring | Not a PID; unique ideal factorization fails, but governed by **primary decomposition** and algebraic geometry. |
| **Formal Power Series Rings ($F[[x]]$)** | Local Ring / UFD | Infinite series expansion; elements are invertible (units) if and only if their constant term is non-zero. |
| **Principal Ideal Domains(PIDs) & Euclidean Domains** | Abstract Algebraic Class | Systems where every ideal is principal (e.g., $ℤ$, $F[x]$), guaranteeing a division algorithm and unique factorization. |
| **$p$-Adic Systems ($ℤ_p, ℚ_p$)** | Complete Valuation Ring / Field | **Inverted:** Only $p$ remains prime; all other standard primes become invertible units. |
| **Modular Rings (ℤ/nℤ)** | Finite Ring | Governed by zero divisors and ideals; primality maps to prime ideals linked to the prime factors of $n$. |
| **Finite Fields ($\mathbb{F}_q$)** | Finite Field | **Element factorization vanishes** (all non-zero elements are units); **polynomial factorization** over $\mathbb{F}_q[x]$ takes center stage. |
| **Number Fields** (e.g., ℚ(√d) ) | Algebraic Number Field | **Field structure:** Element factorization is trivial (all non-zero elements are units). Its associated **Ring of Integers ($𝓞_K$)** governs ideal factorization. |
| **Function Fields** (e.g., $F(t)$ ) | Algebraic Function Field | Geometric twin to number fields; "primes" correspond to points/places on algebraic curves. |
| **Quadratic Integer Rings** (ℤ[√d]) | Ring of Quadratic Integers | Split into **UFDs** (e.g., Gaussian integers $ℤ[i]$) and **Non-UFDs** (e.g., $ℤ[\sqrt{-5}]$ where unique factorization fails). |
| **Rings of Integers ($𝓞_K$)** | Dedekind Domain | Generalization of quadratic rings; element unique factorization fails, but **ideal factorization** always succeeds. |
| **Localizations ($ℤ_{(p)}$)** | Local Ring | Zooms in on a single prime $p$ by making all denominators coprime to $p$ invertible, stripping away other prime factors. |
| **Adele Rings ($𝔸_K$)** | Topological Ring | Glues all local fields ($ℚ_p$ and ℝ/ℂ) together to study global-local arithmetic simultaneously. |
| **Non-Commutative Rings** (e.g., Hurwitz Quaternions) | Non-Commutative Domain | Multiplication order matters ($AB \neq BA$); requires specialized frameworks for left- and right-factorization and primality. |


## Generalization of [Reciprocity Laws](https://en.wikipedia.org/wiki/Reciprocity_law)

[Langlands Reciprocity](https://mathworld.wolfram.com/LanglandsProgram.html) ⊃ [Artin's Reciprocity Theorem](https://mathworld.wolfram.com/ArtinsReciprocityTheorem.html) 
⊃ [Hasse Eisenstein-Kummer Reciprocity](https://en.wikipedia.org/wiki/Eisenstein_reciprocity) ⊃ [Rational Reciprocity Law](https://en.wikipedia.org/wiki/Rational_reciprocity_law) 
⊃ [Quadratic Reciprocity Theorem](https://mathworld.wolfram.com/QuadraticReciprocity.html)

- **[Quadratic Reciprocity Theorem](https://mathworld.wolfram.com/QuadraticReciprocityTheorem.html)**: States solvability conditions of $x^2 \equiv p \pmod{q}$
  and $x^2 \equiv q \pmod{p}$ _only_ when $p$ and $q$ are **_distinct odd primes_**.
  - They are both solvable or both unsolvable unless both $p$ and $q$ leave remainder $3$ when divided by $4$ (in which case one of the congruences is solvable and the other is not).
- **Rational Reciprocity Law**: States solvability conditions of $x^n \equiv p \pmod{q}$ and pre-conditions involving $p$ and $q$ for an $n$ such that $n$ is a power of $2$. Eg:
  - **$(n = 2)$** (This is Quadratic Reciprocity): Gives $P_1$ as the required precondition proposition involving $p$ and $q$. Gives $C_1$ such that
    if $P_1=$`True` then $C_1$ is the condition for solvability.
  - **$(n = 4)$**: Gives $P_2$ as the required precondition proposition involving $p$ and $q$. Gives $C_2$ such that if $P_2=$`True` then $C_2$ is the condition for solvability.
  - **$(n = 8)$**: Gives $P_3$ as the required precondition proposition involving $p$ and $q$. Gives $C_3$ such that if $P_3=$`True` then $C_3$ is the condition for solvability.
  - **$(n = 2^k)$**: Gives $P_k$ as the required precondition proposition involving $p$ and $q$. Gives $C_k$ such that if $P_k=$`True` then $C_k$ is the condition for solvability.
  - and so on.
- **Hasse Eisenstein-Kummer Reciprocity**: States solvability conditions of $x^n \equiv p \pmod{q}$ when $p$ and $q$ are restricted to cyclotomic fields and $n=𝓵$ is a prime power.
- **Artin's Reciprocity Theorem**: Extends reciprocity laws to _all_ **abelian** extensions of _any_ number field (which covers all power residue laws for any $n$)
  eg General Galois groups of extension fields.
- **[Langlands Reciprocity](https://en.wikipedia.org/wiki/Langlands_program#Reciprocity)**: Generalization of Artin reciprocity to **non-Abelian** Galois extensions of number fields.
  - A set of conjectures (many are proven, some are active research areas) about deep connections between:
    - Abstract Algebra (Galois Theory, Field Theory, Group Theory, Number Theory),
    - Analysis (Automorphic Forms), and
    - [Geometry](https://en.wikipedia.org/wiki/Langlands_program#Geometric_conjectures) (See [Geometric Langlands Program](https://handwiki.org/wiki/Geometric_Langlands_correspondence))

## Appendix and References

- [Reciprocity Laws](https://link.springer.com/chapter/10.1007/978-3-662-12893-0_11)
- [Classical Reciprocity Laws](https://www.asiapacific-mathnews.com/04/0404/0005_0009.pdf)
- https://www.sciencedirect.com/topics/mathematics/reciprocity-law
- https://www.math.columbia.edu/~calebji/Reciprocity_laws.pdf
- https://www.math.columbia.edu/~calebji/expository.html
- https://en.wikipedia.org/wiki/Langlands_program
- https://en.wikipedia.org/wiki/Artin_reciprocity
- https://math.stackexchange.com/questions/4485824
- https://math.stackexchange.com/questions/265392
- https://kconrad.math.uconn.edu/blurbs/
- https://kconrad.math.uconn.edu/blurbs/gradnumthy/cfthistory.pdf
- https://euclid.colorado.edu/~grant/publications/publications.html
- https://euclid.colorado.edu/~grant/publications/kummer.pdf
- [Hensel's lemma](https://en.wikipedia.org/wiki/Hensel%27s_lemma)
- https://mathworld.wolfram.com/HenselsLemma.html
- https://www.geeksforgeeks.org/dsa/hensels-lemma/

### Directed Reading Programs
- [Columbia University Math Directed Reading Program (DRP)](https://www.math.columbia.edu/programs-math/undergraduate-program/undergraduate-research/drp/)
- [Stanford Math Directed Reading Program](https://mathdrp.stanford.edu/)
