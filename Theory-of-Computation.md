# Classification of Decision Problems by Decidability & Complexity


$$
\text{RE} \supsetneq \text{R} \supsetneq \text{EXPTIME} \supsetneq \text{PSPACE} \supseteq \text{PP} \supseteq 
\begin{cases}
\text{BQP} \supseteq \text{BPP} \supseteq \text{ZPP} \supseteq \text{P} = \text{co-P} & \qquad \text{(The Quantum/Random Branch)} \\
\text {PH} \supseteq (\text{NP} \stackrel{?}{=} \text{co-NP}) \supseteq \text{P} = \text{co-P} & \qquad \text{(The Non-Deterministic Branch)}
\end{cases}
$$


What is believed (but not proven):

```
                       RE (Recursively Enumerable)
                        ▲
                        │
                        R (Decidable)
                        ▲
                        │
                     EXPTIME
                        ▲
                        │
                     PSPACE ◄────────────── (Space Boundary)
                        ▲
                        │
                       PP (Probabilistic Polynomial / Counting)
                        ▲
        ┌───────────────┴───────────────┐
        │                               │
   (The Quantum/Random Branch)     (The Non-Deterministic Branch)
        │                               │
      BQP                             PH (Polynomial Hierarchy) (Level 2: Σ₂P, Π₂P), (Level 3: Σ₃P, Π₃P), ..., (Level k: ΣₖP, ΠₖP) [For each k: By defn, co-ΣₖP = ΠₖP]
        ▲                               ▲
        │                               │
      BPP                             NP ≟ co-NP (Level 1: Σ₁P (aka NP) ≟ Π₁P (aka co-NP))
        ▲                               │
        │                               │
      ZPP                               │
        │                               │
        └───────────────┬───────────────┘
                        │
                        ▲
              P (Polynomial Time) = co-P (Level 0: Σ₀P (aka P) = Π₀P (aka co-P))
                (The Common Root)
```

## Theorem: $\text{PH} \subseteq \text{PP}$

1. By Toda's Theorem: $\text{PH} \subseteq \text{P}^{\text{PP}}$

2. By Beigel–Reingold–Spielman closure theorem for $\text{PP}$: $\text{P}^{\text{PP}} = \text{PP}$

Therefore, from (1) and (2): $\text{PH} \subseteq \text{P}^{\text{PP}} \implies \text{PH} \subseteq \text{PP}$

## Types of [Computational Problems](https://en.wikipedia.org/wiki/Computational_problem):
- https://en.wikipedia.org/wiki/Decision_problem
  - https://en.wikipedia.org/wiki/Primality_testing
- https://en.wikipedia.org/wiki/Search_problem
  - https://en.wikipedia.org/wiki/Integer_factorization
- https://en.wikipedia.org/wiki/Counting_problem_(complexity)
- https://en.wikipedia.org/wiki/Optimization_problem
- https://en.wikipedia.org/wiki/Function_problem
  - https://en.wikipedia.org/wiki/Travelling_salesman_problem
- https://en.wikipedia.org/wiki/Promise_problem

----

## Appendix and References

- https://en.wikipedia.org/wiki/List_of_data_structures
- https://en.wikipedia.org/wiki/List_of_algorithms
- https://en.wikipedia.org/wiki/List_of_complexity_classes

----

- https://en.wikipedia.org/wiki/Template:Complexity_classes
- https://en.wikipedia.org/wiki/Template:Metalogic
- https://en.wikipedia.org/wiki/Template:Mathematical_logic
- https://en.wikipedia.org/wiki/Computational_complexity_theory
- https://en.wikipedia.org/wiki/Computability_theory
- https://en.wikipedia.org/wiki/Decision_problem
- https://en.wikipedia.org/wiki/Decidability_(logic)
- https://en.wikipedia.org/wiki/Undecidable_problem

----

- https://en.wikipedia.org/wiki/Template:Data_structures_and_algorithms
- https://en.wikipedia.org/wiki/Template:Number-theoretic_algorithms
  - https://en.wikipedia.org/wiki/Miller-Rabin_primality_test
- https://en.wikipedia.org/wiki/Template:Industrial_and_applied_mathematics

----

- https://en.wikipedia.org/wiki/Template:Strings
- https://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm
- https://en.wikipedia.org/wiki/Rabin-Karp_algorithm
