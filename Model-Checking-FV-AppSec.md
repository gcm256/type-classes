# Model Checking & Formal Methods vs InfoSec & AppSec

https://en.wikipedia.org/wiki/Template:Software_development_process

## Model Checking & Formal Methods

- https://en.wikipedia.org/wiki/Template:Mathematical_logic
- https://en.wikipedia.org/wiki/Model_checking
- https://en.wikipedia.org/wiki/Formal_methods
  - https://en.wikipedia.org/wiki/Formal_specification
  - https://en.wikipedia.org/wiki/Formal_verification
    - https://en.wikipedia.org/wiki/Template:Electronic_design_automation
  - https://en.wikipedia.org/wiki/Program_analysis
    - https://en.wikipedia.org/wiki/Template:Program_analysis
- https://en.wikipedia.org/wiki/Proof_assistant
- https://en.wikipedia.org/wiki/Automated_theorem_proving
- https://en.wikipedia.org/wiki/Proof_theory

## Programming Languages

- https://en.wikipedia.org/wiki/Template:Formal_languages_and_grammars
- https://en.wikipedia.org/wiki/Template:Formal_languages
- https://en.wikipedia.org/wiki/Template:Computer_language
- https://en.wikipedia.org/wiki/Template:Programming_languages
- https://en.wikipedia.org/wiki/Template:Types_of_programming_languages
- https://en.wikipedia.org/wiki/Template:Programming_paradigms_navbox
- https://en.wikipedia.org/wiki/Programming_language_theory
- https://en.wikipedia.org/wiki/Template:Semantics
- https://en.wikipedia.org/wiki/Functional_programming
- https://en.wikipedia.org/wiki/Comparison_of_functional_programming_languages
- Lean vs  Coq

## InfoSec

- https://en.wikipedia.org/wiki/Template:Information_security
- https://en.wikipedia.org/wiki/Template:Computer_hacking

## AppSec

- https://en.wikipedia.org/wiki/Application_security
- https://en.wikipedia.org/wiki/OWASP
- https://en.wikipedia.org/wiki/Software_composition_analysis
- https://en.wikipedia.org/wiki/Template:MITRE_security_ontologies
- https://en.wikipedia.org/wiki/Zero-day_vulnerability
- https://en.wikipedia.org/wiki/Zero_trust_architecture

- https://en.wikipedia.org/wiki/Template:Privileged_access_management
- https://blackhat.com/sponsor-posts/08062020-sonatype.html

## AST, SIEM, SOAR, Pen-Test

### SAST+DAST+SCA/OSS:
- BlackDuck SCA
- Burp Suite
- Sonatype
-JFrog Xray

### SAST:
- Fortify
- Coverity
- SonarQube

### DAST:
- Burp Suite DAST

### Container Security:
- Twistlock (now Prisma Cloud)
- Aqua

### AST (Unified SAST + SCA + DAST (plus IaC, containers, secrets scanning)) SaaS platform:
- Black Duck Polaris
- Veracode Platform

### ASPM:
- Cycode

### Pen-Testing / Penetration Testing as a Service (PTaaS) eg [Insecure Direct Object Reference (IDOR)](https://portswigger.net/web-security/access-control/idor):
- Human-led penetration testing remains the gold standard.
  - Burp Suite Professional (PortSwigger)
  - Burp AI
  - Kali Linux & Metasploit Framework (Rapid7)

### Frameworks:
- PCI DSS
- SOC 2
- ISO 27001
- HIPAA

----

## Summary Overview: Proof Assistants & Application Security

----

### 1. Formal Verification: Lean 4 vs. Coq (Rocq)

| Dimension | Lean (Lean 4) | Coq (Rocq) |
| :--- | :--- | :--- |
| **Why It Was Created** | Built by Leonardo de Moura (2013) to unify interactive proving, automated theorem proving (SMT-style), and modern software engineering. | Pioneered dependent type theory for formal logic, programming language semantics, and verified software. |
| **Primary Strength** | Modern pure mathematics (`mathlib`), AI-assisted proving, and general-purpose functional programming. | Battle-tested systems and compiler verification (e.g., **CompCert**, **CertiKOS**, **Iris**). |
| **Tactic & Extension Language** | **Lean itself** (unified metaprogramming in a single compiled language). | Separate tactic languages (**Ltac / Ltac2**) and **OCaml** plugins. |
| **Math Ecosystem** | Single, unified community library (`mathlib`) with strong classical math ergonomics. | Multiple distinct ecosystems (Standard Library, MathComp, UniMath). |
| **Execution Model** | Self-hosted; compiles directly to C / native binaries. | Interpreted or extracted to OCaml / Haskell / Scheme. |

----

### 2. Application Security Testing (AST) Taxonomy

Modern AppSec divides testing into distinct layers rather than one single tool type:

| Category | Full Name | What It Inspects | Requires Running App? | Common Misconceptions |
| :--- | :--- | :--- | :--- | :--- |
| **SAST** | Static Application Security Testing | Your proprietary source code, bytecode, or binaries (*white-box*). | No | Does not cover third-party dependencies on its own. |
| **DAST** | Dynamic Application Security Testing | Live, deployed web apps and APIs from the outside over HTTP/HTTPS (*black-box*). | Yes | Not the same as full manual penetration testing. |
| **SCA** | Software Composition Analysis | Third-party open-source packages, dependencies (OSS), and SBOMs for CVEs and license risks. | No | Separate from both SAST and DAST; **OSS** is what SCA scans, not a testing type. |
| **ASPM** | Application Security Posture Management | Aggregates findings across SAST, SCA, DAST, IaC, secrets, and cloud runtime to map risk and prioritize fixes. | No (correlates across SDLC) | sits *above* individual scanners as a control plane and risk graph. |

----

### 3. AppSec Vendor & Platform Landscape

#### Point Tools vs. Unified Suites
* **SonarQube:** Primarily **SAST** and code quality/linters (requires external tools/plugins for full SCA).
* **Coverity (Black Duck):** Strictly **SAST** and static code quality analysis.
* **Sonatype (Lifecycle / Nexus):** Dedicated **SCA** and software supply-chain governance (not DAST).
* **Black Duck Polaris:** Unified **SaaS AST suite** bundling **SAST** (*Coverity*), **SCA** (*Black Duck*), and **DAST** (*fAST Dynamic*), plus IaC and secrets scanning.

### Notable Tools by Category

1. **Dedicated SCA Tools:**
   * *Enterprise / Commercial:* Sonatype Lifecycle, Black Duck SCA, Mend.io, Endor Labs, JFrog Xray, FOSSA.
   * *Developer-First / Built-In:* Snyk Open Source, GitHub Dependabot, Semgrep Supply Chain.
   * *Free / Open-Source:* Trivy, Grype + Syft, OWASP Dependency-Check, OWASP Dependency-Track, Google OSV-Scanner.

2. **DAST Tools:**
   * *Open-Source / Free:* OWASP ZAP, Nuclei, Burp Suite Community Edition.
   * *Commercial / Enterprise:* Burp Suite DAST, Invicti (Netsparker), Acunetix, StackHawk, Bright Security, Rapid7 InsightAppSec, Qualys WAS, Fortify WebInspect.

3. **Unified SAST + SCA + DAST (+ IaC, Containers, Secrets) SaaS Platforms:**
   * Black Duck Polaris, Checkmarx One, Veracode Platform, OpenText Fortify on Demand, Snyk, GitLab Ultimate, Aikido Security, Mend.io.

4. **ASPM Platforms:**
   * *Pure-Play / Supply-Chain ASPM:* **Cycode** (ASPM + native SAST/SCA/secrets/IaC/CI-CD scanners), **Apiiro**, **ArmorCode**, **Legit Security**, **OX Security**, **DefectDojo**.
   * *CNAPP + ASPM:* **Wiz (Wiz Code)**, **Orca Security**, **Palo Alto Cortex/Prisma Cloud**, **CrowdStrike Falcon ASPM**.
   * *AST Suite + ASPM:* **Snyk AppRisk**, **Checkmarx One ASPM**, **Veracode Risk Manager**.

----

### 4. Penetration Testing

* **Automated DAST vs. Penetration Testing:** Automated DAST broadly scans known vulnerability signatures, whereas penetration testing actively exploits weaknesses, chains low-severity flaws, uncovers business-logic/authorization abuse (like IDOR), and satisfies compliance audits (SOC 2, PCI DSS, ISO 27001, HIPAA).
* **Role of Human Testers:** Human-led pentesting remains the gold standard for complex business logic and formal attestation, though modern teams combine it with continuous AI/automated testing to avoid once-a-year blind spots.
* **Leading Pen-Testing Tools, Platforms & Services:**
  * **Manual Pentester Toolkits:** Burp Suite Professional, Kali Linux, Metasploit Framework, Nuclei, BloodHound / Impacket.
  * **PTaaS (Penetration Testing as a Service — Human + Cloud):** Synack, Cobalt, HackerOne, Bugcrowd, NetSPI, BreachLock, Astra Security (also offered as services by Veracode and Black Duck).
  * **Autonomous AI / Continuous Attack Simulation:** Horizon3.ai (NodeZero), Pentera, Escape, XBOW, Aikido Attack.
  * **Boutique & Enterprise Consultancies:** Bishop Fox, NCC Group, Coalfire, SpecterOps, OffSec.
 
----
----
