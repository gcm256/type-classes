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

[Langlands Conjectures](https://mathworld.wolfram.com/LanglandsProgram.html) ⊃ [Artin's Reciprocity Theorem](https://mathworld.wolfram.com/ArtinsReciprocityTheorem.html) 
⊃ [Hasse Eisenstein-Kummer Reciprocity](https://en.wikipedia.org/wiki/Eisenstein_reciprocity) ⊃ [Quadratic Reciprocity Theorem](https://mathworld.wolfram.com/QuadraticReciprocity.html)

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
