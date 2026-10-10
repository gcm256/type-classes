# Theory of Languages, Scripts, Speech, Phonetics

See: [^15][^16]

## Linguistic Units

- Grapheme
- Phoneme
  - Viseme (The smallest visible distinct mouth-shape when producing a Phoneme). Phoneme → Viseme mapping is strictly Many-One, since multiple Phonemes
    can be produced by the same visible mouth-shape, but multiple visible mouth-shapes cannot really produce the same phoneme.
- Lexeme
- Morpheme
- Sememe
- Toneme
- Tagmeme
- Glosseme


## Morphology

### Root vs Stem

### Inflection[^20]

- **Conjugation:** Inflection of verbs (Tinganta / तिङन्तम्)[^19]
- **Declension:** Inflection of nominals (Subanta / सुबन्तम्)[^18]

## Phonetics[^1][^2]

- Fricative: Friction in airflow.
  - Affricate: Stop + Fricative
- Plosive: Neither Fricative nor Affricate eg क, ख etc.
- Sibilant: The hissing sound of a consonant. Sibilants can only be either Fricative or Affricate.
  - Sibilant Fricative: स, श, ष
  - Sibilant Affricate: च, छ, ज, झ
  - Non-sibilant Fricative: Friction in airflow, but no hissing sound. Denoted by using a Nukta (dot) diacritic (़) Eg ख़, ग़ etc
    - Anything non-sibilant with a Visarga (ः) eg तः
    - Also, ह
  - Non-sibilant Affricate: Very rare. Eg क्ख़ (क् + ख़) etc.

### The Master Cross-Reference Matrix
This matrix categorizes the entire core Devanagari script, standard Nukta additions, and structural edge cases (Sindhi Implosives and Ingressive Clicks) strictly by Airstream, Phonation, and Manner, omitting the place of articulation completely.


| Airstream Mechanism | Phonation (Voicing) | Aspiration Profile | Manner of Articulation | Included Characters / Alphabet Set |
| :--- | :--- | :--- | :--- | :--- |
| **Pulmonic Egressive** *(Outward Lung Air)* | **Voiceless** | **Unaspirated** | Plosive | क, ट, त, प, क़ [q] |
| | | | Affricate | च |
| | | | Fricative | ज़ |
| | | **Aspirated** | Plosive | ख, ठ, थ, फ |
| | | | Affricate | छ |
| | | | Fricative (Sibilant) | श, ष, स |
| | | | Fricative (Non-Sibilant)| ख़, फ़, ः *(Visarga)* |
| | **Voiced** | **Unaspirated** | Plosive | ग, ड, द, ब |
| | | | Affricate | ज |
| | | | Fricative | ग़ |
| | | | Nasal | ङ, ञ, ण, न, म, ं *(Anusvara)*, ँ *(Chandrabindu)* |
| | | | Approximant / Semivowel| य, व |
| | | | Vibrant (Trill / Lateral)| र, ल |
| | | | Flap | ड़ |
| | | | Vowels (All Vowels) | अ, आ, इ, ई, उ, ऊ, ऋ, ॠ, ऌ, ए, ऐ, ओ, औ |
| | | **Aspirated** *(Breathy)*| Plosive | घ, ढ, ध, भ |
| | | | Affricate | झ |
| | | | Fricative (Glottal) | ह |
| | | | Flap | ढ़ |
| **Ingressive Implosive** *(Inward Throat Vacuum)* | **Voiced** | **Unaspirated** | Plosive | ग॒, ज॒, ड॒, ब॒ *(Sindhi Implosives)* |
| **Ingressive Click** *(Inward Mouth Vacuum)* | **Voiceless / Voiced**| **Unaspirated / Aspirated**| Fricative / Lateral Click| *None in Devanagari* *(e.g., Xhosa Clicks)* |

#### Example

**Ordering of Articulation[^1][^2]:** \
Voicing: Voiced/Voiceless (✅/❌), \
Aspiration: Aspirated/Unaspirated (✅/❌), \
Plosive/Fricative\[Sibilant\]/Affricate\[Sibilant\]/Nasal/Rhotic-Liquid-Tap-Flap/Rhotic-Liquid-Trill/Lateral-Liquid/Vowel/Semivowel (P/F\[s\]/A\[s\]/N/R/Rr/L/V/V*), \
Airstream: Ingressive aka Non-Egressive (🟦)

| अ | आ | इ | ई | उ | ऊ |
|:---:|:---:|:---:|:---:|:---:|:---:|
| ए <br> (अ/आ + इ/ई) | ऐ <br> (अ/आ + ए) | ओ <br> (अ/आ + उ/ऊ) | औ <br> (अ/आ + ओ) | ऋ, ॠ | ऌ |


| क (❌, ❌, P) <br> क़ (❌, ❌, P) <br> <ins>क़</ins> (❌, ❌, F) | च (❌, ❌, As) <br><br><br> | ट (❌, ❌, P) <br><br><br> | त (❌, ❌, P) <br><br><br> | प (❌, ❌, P) <br><br><br> |
|:---|:---|:---|:---|:---|
| ख (❌, ✅, P) <br> ख़ (❌, ✅, F) <br> क्ख़ (❌, ✅, A) | छ (❌, ✅, As) <br><br><br> | ठ  (❌, ✅, P) <br><br><br> | थ (❌, ✅, P) <br><br><br> | फ (❌, ✅, P) <br> फ़ (❌, ✅, F) <br><br> |
| ग (✅, ❌, P) <br> ग़ (✅, ❌, F) <br> ग॒ (✅, ❌, P, 🟦) | ज (✅, ❌, As) <br> ज़ (✅, ❌, Fs) <br> ज॒ (✅, ❌, As, 🟦) | ड (✅, ❌, P) <br> ड़ (✅, ❌, R) <br> ड॒ (✅, ❌, P, 🟦) | द (✅, ❌, P) <br><br><br> | ब (✅, ❌, P) <br><br> ब॒ (✅, ❌, P, 🟦) |
| घ (✅, ✅, P) <br><br> | झ (✅, ✅, As) <br><br> | ढ (✅, ✅, P) <br> ढ़ (✅, ✅, R) | ध (✅, ✅, P) <br><br> | भ (✅, ✅, P) <br><br> |
| ङ (✅, ❌, N) | ञ (✅, ❌, N) | ण (✅, ❌, N) | न (✅, ❌, N) | म (✅, ❌, N) |
| र (✅, ❌, Rr) | ल (✅, ❌, L) | ळ (✅, ❌, L) | य (✅, ❌, V*) | व (✅, ❌, V*) |
| श (❌, ✅, Fs) | ष (❌, ✅, Fs) | स (❌, ✅, Fs) |  |   |
| ह (✅, ✅, F) | | | | |
| ं (✅, ❌, P, N) | ँ (✅, ❌, P, N) | ः (❌, ❌, P, F) | | |

> [!NOTE]
> Ingressive ie non-Egressive sounds, by definition, cannot be Aspirated.
> - Because non-Egressive means air is drawn in. But Aspirated requires a high-volume outward puff of air, which contradicts non-Egression.
>
> Ingressive ie non-Egressive sounds in Indic languages only occur in Sindhi.
> - The characters ग॒, ज॒, ड॒, ब॒ are specialized Devanagari characters for Plosive non-Egressive sounds occurring in Sindhi language.
>   - Airstream is Ingressive ie non-Egressive, ie air is drawn/sucked/gulped in and flows inward, instead of outward.
>   - They are all Plosive (ie Stop Consonants).
> - There are no non-Plosive non-Egressive sounds (that are used in actual words) in Indic languages, or any other major languages except some African and Indigenous Australian languages.
>   - Non-Plosive non-Egressive sounds will sound like:
>     - Clicks. 
>     - The "tsk-tsk" sound of disapproval.
>     - Snoring (when the snorer breathes in).
>   - Even if not in actual words, still we do use "tsk-tsk" paraliguistically to express an emotion, and clicks eg to urge on horses and cattle.
> - All other Indic language characters and sounds are Egressive where air is pushed out and flows outward.
>   

## Appendix and References

See: [^3][^4][^5][^6][^7][^8][^9][^10][^11][^12][^13][^14][^17]

[^1]: https://en.wikipedia.org/wiki/Template:Articulation_navbox
[^2]: https://en.wikipedia.org/wiki/Template:IPA_navigation
[^3]: https://en.wikipedia.org/wiki/Portal:Linguistics
[^4]: https://en.wikipedia.org/wiki/Zero_(linguistics)
[^5]: https://en.wikipedia.org/wiki/Linguistics
[^6]: https://en.wikipedia.org/wiki/Functional_linguistics
[^7]: https://en.wikipedia.org/wiki/Morphology_(linguistics)
[^8]: https://en.wikipedia.org/wiki/Category:Linguistics
[^9]: https://en.wikipedia.org/wiki/Computational_linguistics
[^10]: https://en.wikipedia.org/wiki/Mathematical_linguistics
[^11]: https://en.wikipedia.org/wiki/Index_of_linguistics_articles
[^12]: https://en.wikipedia.org/wiki/Outline_of_linguistics
[^13]: https://en.wikipedia.org/wiki/Root_(linguistics)
[^14]: https://en.wikipedia.org/wiki/Compound_(linguistics)
[^15]: ⏯️ [MIT 24.900 Introduction to Linguistics, Spring 2022](https://www.youtube.com/playlist?list=PLUl4u3cNGP63BZGNOqrF2qf_yxOjuG35j)
[^16]: ⏯️ [Richards - Linguistics (2022)](https://www.youtube.com/playlist?list=PLmsIjFudc1l0baz7D-oRF1jc1F1kDfGm8)

[^18]: https://en.wikipedia.org/wiki/Sanskrit_nominals \
https://dharmawiki.org/index.php/Subanta_(सुबन्तम्) \
https://sa.wikipedia.org/wiki/सुबन्तम्
[^19]: https://en.wikipedia.org/wiki/Sanskrit_verbs \
https://dharmawiki.org/index.php/Tinganta_(तिङन्तम्)
[^20]: https://sa.wikipedia.org/wiki/वर्गः:संस्कृतव्याकरणम् \
https://en.wikipedia.org/wiki/Sanskrit_grammar

[^17]: https://en.wikipedia.org/wiki/Formal_language
https://en.wikipedia.org/wiki/Category:Formal_languages
https://en.wikipedia.org/wiki/Category:Constructed_languages
https://en.wikipedia.org/wiki/Ithkuil
https://en.wikipedia.org/wiki/List_of_constructed_languages
https://en.wikipedia.org/wiki/Constructed_language
https://en.wikipedia.org/wiki/Engineered_language
https://en.wikipedia.org/wiki/Controlled_natural_language
https://en.wikipedia.org/wiki/Controlled_vocabulary
https://en.wikipedia.org/wiki/Simplified_Technical_English
https://en.wikipedia.org/wiki/Linguistic_relativity
https://en.wikipedia.org/wiki/Hopi_time_controversy
https://en.wikipedia.org/wiki/Linguistic_determinism
https://en.wikipedia.org/wiki/Language_and_thought
https://en.wikipedia.org/wiki/Experimental_language
https://en.wikipedia.org/wiki/List_of_constructed_languages#Engineered_languages
https://en.wikipedia.org/wiki/Lists_of_languages
https://en.wikipedia.org/wiki/Programming_language
https://en.wikipedia.org/wiki/Synthetic_language
https://en.wikipedia.org/wiki/Analytic_language
https://en.wikipedia.org/wiki/Isolating_language
https://en.wikipedia.org/wiki/Polysynthetic_language
https://en.wikipedia.org/wiki/Morphological_typology
https://en.wikipedia.org/wiki/Linguistic_typology
https://en.wikipedia.org/wiki/Agglutinative_language
https://en.wikipedia.org/wiki/Zero-marking_language
https://en.wikipedia.org/wiki/Word_order
https://en.wikipedia.org/wiki/Semantics
https://en.wikipedia.org/wiki/Formal_grammar
https://en.wikipedia.org/wiki/Unrestricted_grammar
https://en.wikipedia.org/wiki/Recursively_enumerable_language
https://en.wikipedia.org/wiki/Context-free_grammar
https://en.wikipedia.org/wiki/Recursive_grammar
https://en.wikipedia.org/wiki/Morphology_(linguistics)
https://en.wikipedia.org/wiki/Morpheme
https://en.wikipedia.org/wiki/Grammatical_category
https://en.wikipedia.org/wiki/Word_formation
https://en.wikipedia.org/wiki/Apophony
https://en.wikipedia.org/wiki/Universal_grammar
https://en.wikipedia.org/wiki/Phoneme
https://en.wikipedia.org/wiki/Glyph
https://en.wikipedia.org/wiki/Grapheme
https://en.wikipedia.org/wiki/Typography
https://en.wikipedia.org/wiki/Lexicology
https://en.wikipedia.org/wiki/Formal_semantics_(natural_language)
https://en.wikipedia.org/wiki/Semantics_of_logic
https://en.wikipedia.org/wiki/Language
https://en.wikipedia.org/wiki/Allophone
https://en.wikipedia.org/wiki/Semantic_class
https://en.wikipedia.org/wiki/Kenning
https://en.wikipedia.org/wiki/Syntax
https://en.wikipedia.org/wiki/Syntax-semantics_interface
https://en.wikipedia.org/wiki/Head-marking_language
https://en.wikipedia.org/wiki/Dependency_grammar
https://en.wikipedia.org/wiki/Word_stem
https://en.wikipedia.org/wiki/Sandhi
https://en.wikipedia.org/wiki/Inflection
https://en.wikipedia.org/wiki/Grammatical_conjugation
https://en.wikipedia.org/wiki/Declension
https://en.wikipedia.org/wiki/Grammatical_relation
https://en.wikipedia.org/wiki/Affix
https://en.wikipedia.org/wiki/Bound_and_free_morphemes
https://en.wikipedia.org/wiki/Separable_verb
https://en.wikipedia.org/wiki/Word_family
https://en.wikipedia.org/wiki/Content_word
https://en.wikipedia.org/wiki/Lemma_(morphology)
https://en.wikipedia.org/wiki/Sanskrit_compound
https://en.wikipedia.org/wiki/Writing_system
https://en.wikipedia.org/wiki/Abjad
https://en.wikipedia.org/wiki/Abugida
https://en.wikipedia.org/wiki/List_of_constructed_scripts
https://en.wikipedia.org/wiki/Constructed_writing_system
https://en.wikipedia.org/wiki/Asemic_writing
https://bharatiscript.com/
https://en.wikipedia.org/wiki/Null-subject_language
https://en.wikipedia.org/wiki/Vowel_harmony
https://en.wikipedia.org/wiki/Phonology
https://en.wikipedia.org/wiki/Agglutination
https://en.wikipedia.org/wiki/T-V_distinction



