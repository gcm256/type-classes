# Factorization and Primality Across Number Systems, Algebraic Structures, Rings, and Fields

| Structure / System | Classification | Factorization & Primality Behavior |
| :--- | :--- | :--- |
| **Positional Bases** (e.g., Binary, Decimal) | Notation / Representation | **Invariant:** Changes digit representation, but leaves the underlying arithmetic and primes untouched. |
| **Standard Integers ($\mathbb{Z}$)** | Unique Factorization Domain (UFD) | Standard unique prime factorization (Fundamental Theorem of Arithmetic). |
| **Polynomial Rings ($F[x]$)** | Euclidean Domain (UFD) | Primality translates to **irreducible polynomials**; unique factorization holds analogously to $\mathbb{Z}$. |
| **Multivariate Polynomials ($F[x_1, \dots, x_n]$)** | Noetherian Ring | Not a PID; unique ideal factorization fails, but governed by **primary decomposition** and algebraic geometry. |
| **Formal Power Series Rings ($F[[x]]$)** | Local Ring / UFD | Infinite series expansion; elements are invertible (units) if and only if their constant term is non-zero. |
| **Principal Ideal Domains(PIDs) & Euclidean Domains** | Abstract Algebraic Class | Systems where every ideal is principal (e.g., $\mathbb{Z}$, $F[x]$), guaranteeing a division algorithm and unique factorization. |
| **$p$-Adic Systems ($\mathbb{Z}_p, \mathbb{Q}_p$)** | Complete Valuation Ring / Field | **Inverted:** Only $p$ remains prime; all other standard primes become invertible units. |
| **Modular Rings ($\mathbb{Z}/n\mathbb{Z}$)** | Finite Ring | Governed by zero divisors and ideals; primality maps to prime ideals linked to the prime factors of $n$. |
| **Finite Fields ($\mathbb{F}_q$)** | Finite Field | **Element factorization vanishes** (all non-zero elements are units); **polynomial factorization** over $\mathbb{F}_q[x]$ takes center stage. |
| **Number Fields** (e.g., $\mathbb{Q}(\sqrt{d})$ ) | Algebraic Number Field | **Field structure:** Element factorization is trivial (all non-zero elements are units). Its associated **Ring of Integers ($\mathcal{O}_K$)** governs ideal factorization. |
| **Function Fields** (e.g., $F(t)$ ) | Algebraic Function Field | Geometric twin to number fields; "primes" correspond to points/places on algebraic curves. |
| **Quadratic Integer Rings ($\mathbb{Z}[\sqrt{d}]$)** | Ring of Quadratic Integers | Split into **UFDs** (e.g., Gaussian integers $\mathbb{Z}[i]$) and **Non-UFDs** (e.g., $\mathbb{Z}[\sqrt{-5}]$ where element factorization fails). |
| **Rings of Integers ($\mathcal{O}_K$)** | Dedekind Domain | Generalization of quadratic rings; element unique factorization fails, but **ideal factorization** always succeeds. |
| **Localizations ($\mathbb{Z}_{(p)}$)** | Local Ring | Zooms in on a single prime $p$ by making all denominators coprime to $p$ invertible, stripping away other prime factors. |
| **Adele Rings ($\mathbb{A}_K$)** | Topological Ring | Glues all local fields ($\mathbb{Q}_p$ and $\mathbb{R}/\mathbb{C}$) together to study global-local arithmetic simultaneously. |
| **Non-Commutative Rings** (e.g., Hurwitz Quaternions) | Non-Commutative Domain | Multiplication order matters ($AB \neq BA$); requires specialized frameworks for left- and right-factorization and primality. |
