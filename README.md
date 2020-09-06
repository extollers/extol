# Extol

Extol is a fifth-generation programming language being bootstrapped on top of Prolog.

## Links

- [Source Code](https://github.com/extollers/extol)
- [Repl.it](https://repl.it/github/extollers/extol)
- [Reddit](https://www.reddit.com/r/extollers/)
- [~Wikipedia~](https://en.wikipedia.org/wiki/Extol_(programming_language))
- [~Rosetta Code~](https://rosettacode.org/wiki/Category:Extol)
- [~Stack Overflow~](https://stackoverflow.com/questions/tagged/extol)
- [~Trending on GitHub~](https://github.com/trending/extol)
- [~rise4fun~](https://rise4fun.com/Extol)

# Inspiration

- [Mercury](http://www.mercurylang.org/):
  Combining the power of Haskell and Prolog

- [CiaoPP](https://ciao-lang.org/):
  Type inference, static debugging and program transformation as a preprocessor

- [Picat](http://picat-lang.org/) and [Rosette](https://docs.racket-lang.org/rosette-guide/index.html):
  Constraint solving as a primitive

- [Lean](https://leanprover.github.io/):
  A common language for programming, metaprogramming and theorem proving

- [Cecil](http://projectsweb.cs.washington.edu/research/projects/cecil/www/Release/index.html):
  Predicate objects

# Roadmap

- [x] A Prolog parser in Prolog that can parse itself
- [x] A Prolog generator for the parsed declarations
- [x] Improved syntax and semantics
- [x] Runtime type and contract checking
- [x] A REPL
- [x] Emacs mode
- [x] Include statement
- [x] Functions and expressions instead of predicates and goals
- [x] Stack traces
- [x] Clause transformation by annotation
- [ ] Indentation-sensitive syntax (get rid of those parentheses)
- [ ] Proper AST instead of raw terms (to allow better type checking)
- [ ] `nondet` clause annotations (to improve performance and reasoning)
- [ ] Replace `,` with `do` blocks
- [ ] Add `where` clauses
- [ ] Static type checking
- [ ] Explicit import of external functions
- [ ] Anonymous functions, lambdas and closures
- [ ] First-class functions
- [ ] Modules and namespaces
- [ ] Improved error messages
- [ ] Compile-time type checking
- [ ] Unicode
- [ ] Open sets
- [ ] Constraint solving
- [ ] Termination checking
- [ ] Theorem proving
- [ ] Parallelism
- [ ] Effects
- [ ] A faster backend