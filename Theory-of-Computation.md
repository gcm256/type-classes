# Classification of Decision Problems by Decidability & Complexity


$$
\text{RE} \supsetneq \text{R} \supsetneq \text{EXPTIME} \supsetneq \text{PSPACE} \supseteq \text{PP} \supseteq 
\begin{Bmatrix*}[l|c|c|c|c|c|c|c|c|c|c|c|r]
\text{BQP} & & & & & & & \supseteq & (\text{BPP} = \text{co-BPP}) & \supseteq & (\text{RP} \stackrel{?}{=} \text{co-RP})  & & & \qquad \text{(The Quantum/Random Branch)} \\
\text {PH} & & & & & & & \supseteq & (\text{Σ₂P} ∩ \text{Π₂P}) & \supseteq & (\text{BPP} = \text{co-BPP}) & & \\
\text {PH} & \supseteq & \text{ΣₖP} & \supseteq & \cdots & \supseteq & \text{Σ₃P} & \supseteq & \text{Σ₂P} & \supseteq & \text{NP} & \supseteq & \text{RP} & \qquad \text{(The Non-Deterministic Branch)} \\
& & & & & & & & & & \stackrel{?}{=} & & \stackrel{?}{=} \\
\text {PH} & \supseteq & \text{ΠₖP} & \supseteq & \cdots & \supseteq & \text{Π₃P} & \supseteq & \text{Π₂P} & \supseteq & \text{co-NP} & \supseteq & \text{co-RP} & \qquad \text{(The Non-Deterministic Branch)}
\end{Bmatrix*}
\supseteq (\text{ZPP ≝ RP ∩ co-RP}) \supseteq (\text{P} = (\text{co-P})
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
        |                               |
   (The Quantum/Random Branch)     (The Non-Deterministic Branch)                               
        │                               │
        |                           PH (Polynomial Hierarchy) [For each k: By defn, co-ΣₖP = ΠₖP]
        |                               |    \
        |                               ΣₖP  ΠₖP   (Level k: ΣₖP, ΠₖP)
      BQP                               ▲     ▲ 
        ▲                               |     |
        │                               .     .
        |                               .     .
        |                               Σ₃P  Π₃P   (Level 3: Σ₃P, Π₃P)
        │                               ▲     ▲
        |                               |     |
        |            ┌───────────────▶︎  Σ₂P  Π₂P  (Level 2: Σ₂P, Π₂P)
        |            |                  ▲     ▲
        |            |                  |     |
        |  ┌───▶︎ Σ₂P ∩ Π₂P              NP ≟ co-NP (Level 1: Σ₁P (aka NP) ≟ Π₁P (aka co-NP))
        | /                             ▲     ▲
        │/                              │     │
      BPP = co-BPP                      |     |
        ▲                               |     |
        |                               |     |
        └────────────────────────────── RP ≟ co-RP
        └─────────────────────────────────────┘
                                        ▲     ▲
                                         \   /
                                          ZPP ≝ RP ∩ co-RP
                                           ▲
                                           |
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

- https://en.wikipedia.org/wiki/ZPP_(complexity)
- https://en.wikipedia.org/wiki/RP_(complexity)
- https://en.wikipedia.org/wiki/BPP_(complexity)
- https://en.wikipedia.org/wiki/Polynomial_hierarchy
- https://en.wikipedia.org/wiki/Exponential_hierarchy

----

- https://en.wikipedia.org/wiki/Template:Data_structures_and_algorithms
- https://en.wikipedia.org/wiki/Template:Number-theoretic_algorithms
  - https://en.wikipedia.org/wiki/Miller-Rabin_primality_test
- https://en.wikipedia.org/wiki/Template:Industrial_and_applied_mathematics
- https://en.wikipedia.org/wiki/Probabilistic_algorithm
- https://en.wikipedia.org/wiki/Template:Probabilistic
- https://en.wikipedia.org/wiki/Randomized_algorithm
- https://en.wikipedia.org/wiki/Approximation_algorithm
- https://en.wikipedia.org/wiki/Template:Parsers
- https://en.wikipedia.org/wiki/Template:Optimization_algorithms
- [Smallest-circle problem](https://en.wikipedia.org/wiki/Smallest-circle_problem)
- [Moser's circle problem](https://en.wikipedia.org/wiki/Moser%27s_circle_problem)
- https://en.wikipedia.org/wiki/Mathematical_optimization#Major_subfields
- https://en.wikipedia.org/wiki/Template:Evolutionary_computation
- https://en.wikipedia.org/wiki/Template:Graph_traversal_algorithms
- https://en.wikipedia.org/wiki/Template:Artificial_intelligence_algorithms
- https://en.wikipedia.org/wiki/Template:Decision_theory
- https://en.wikipedia.org/wiki/Template:Game_theory

----

- https://en.wikipedia.org/wiki/Template:Strings
- https://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm
- https://en.wikipedia.org/wiki/Rabin-Karp_algorithm

----

- https://en.wikipedia.org/wiki/Probabilistic_method
- https://en.wikipedia.org/wiki/Algorithmic_probability
- https://en.wikipedia.org/wiki/Probabilistic_analysis_of_algorithms
- https://en.wikipedia.org/wiki/Algorithmic_information_theory
- https://en.wikipedia.org/wiki/Template:Statistics
- https://en.wikipedia.org/wiki/Probabilistic_encryption

----

- [Operations Research](https://en.wikipedia.org/wiki/Operations_research)
- https://en.wikipedia.org/wiki/LP-type_problem
- https://en.wikipedia.org/wiki/Linear_programming
- https://en.wikipedia.org/wiki/Nonlinear_programming
- https://en.wikipedia.org/wiki/Computational_geometry#See_also
- https://en.wikipedia.org/wiki/Template:Systems_engineering
- [Systems Theory](https://en.wikipedia.org/wiki/Systems_theory)
- https://en.wikipedia.org/wiki/Template:Systems_science
- https://en.wikipedia.org/wiki/Template:Complex_systems
- https://en.wikipedia.org/wiki/Template:Complex_systems_topics
- https://en.wikipedia.org/wiki/Complex_system#See_also
- https://en.wikipedia.org/wiki/Nonlinear_system
- https://en.wikipedia.org/wiki/Complex_adaptive_system
- [Cybernetics](https://en.wikipedia.org/wiki/Cybernetics)
- https://en.wikipedia.org/wiki/Template:Cybernetics
- https://en.wikipedia.org/wiki/Template:Mathematical_programming
- https://en.wikipedia.org/wiki/Template:Areas_of_mathematics
