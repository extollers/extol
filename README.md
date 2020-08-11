[![Run on Repl.it](https://repl.it/badge/github/atnnn/extol)](https://repl.it/github/atnnn/extol)

# Inspiration

- [Mercury](http://www.mercurylang.org/)
- [Ciao](https://ciao-lang.org/)
- [Picat](http://picat-lang.org/)
- [Lean](https://leanprover.github.io/)
- [Rosette](https://docs.racket-lang.org/rosette-guide/index.html)

# Roadmap

- a bootstrapped high-level declarative language (code name Extol)
  - ☑ a Prolog parser in Prolog that can parse itself
  - ☑ a Prolog generator for the parsed declaration
  - ☑ runtime type checking
  - ☑ repl
  - → explicit import
  - □ stack traces
  - □ compile-time type checking
  - □ proper AST instead of raw Prolog terms
  - □ custom prelude to wrap Prolog predicates
  - □ functions and expressions similar to Ciao's fsyntax
  - □ anonymous predicates similar to Mercury
  - □ first-class predicates and functions
  - □ modules and namespaces

(□ to do, → in progress, ☑ done)

# Further ideas

- explicit lexical elements
- extensible lexical elements
- middle-out parsing
- → parser for C
- types and constraints
- Unicode
- rich library of data types
- non-binary const, open sets and maps
- parser for Python
- parser for JavaScript
- parser for Bash
- constraint and logic programming
- compiling interpreted scripts by tree shaking down to C
- convert between languages
- polyglot library for common types and functions
- polyglot FFI and messaging
- Convert C++ to C-ish: instantiate all templates, explicit construct, destroy, copy, move, convert and return
- theorem proving
- SMT
- parallelism
