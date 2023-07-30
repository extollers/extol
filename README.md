# The Extol Programming Language

The Extol programming language is an early work-in-progress. It is
currently being bootstrapped from Prolog.

The goals of this project are:

- A uniform language for values, function, statements, expressions,
  types, kinds, macros, patterns, constraints, grammars, tactics and
  proofs

- A flexible syntax that allows building incrementally typed
  high-level shell-style scripts, low-level systems code and
  domain-specific languages

- A type system with small-core verifiable theorem proving,
  incremental typing, deferred type errors, termination proofs, no
  universe hierarchy and separate types for compile-time (semantics)
  and run-time (representation)

- Semantics that allow strict/lazy/reactive/mixed evaluation,
  value/reference/move parameters, prompt finalization, optional
  garbage collection, mutable/immutable/const/nondet/constraint
  variables, weak/lazy/mutable/immutable/serializable types,
  backtracking, (un)delimited continuations, resumable exceptions,
  static/dynamic polymorphism and static/dynamic/multiple dispatch

- A composable environment and effects system that allows hopping
  between abstraction levels, powerful negotiated implicit arguments
  and detailed introspection of program requirements and behavior

- A portable self-hosted interpreter and cross-compiler that targets
  x86_64, ARM, C, .NET, JVM, WebAssembly and SPIR-V

- An FFI that can import and export to and from C, C++, Python,
  JavaScript, Ruby, Perl, sh, Java, C#, Go, Rust, R, ELisp and Nix

- Parallel, distributed, heterogenous, symbolic and partial execution

- Built-in cross-platform support for file IO, declarative UI,
  networking, HTTP, HTML, JSON, IPC, Unicode, math, times and dates,
  regular expressions, concurrency, string formatting, serialization,
  cryptography and testing

- A module and packaging system for code sharing and re-use

## Documentation

<details><summary>

### Quick Start

</summary><figure>

#### Setup Extol

Get the latest source code:

```
git clone https://github.com/extollers/extol
cd extol
```

Install the dependencies. For example, on Ubuntu:

```
sudo apt install gprolog
```

Or with Nix:

```
nix develop
```

Build the compiler and install it to `./local`:

```
make install
```

Optionally, run all tests:

```
make check
```

#### Using the REPL

```
$ ./local/bin/extol repl

Extol> 1 + 1
2

Extol> 'Hello, world!'
Hello, world!
```

#### Using the compiler

```
$ cat > hello.xtl
pred main: ():
  write('Hello, world!'), nl,
  halt.

$ ./local/bin/extol extoltoprolog hello.xtl hello.prolog

$ gplc hello.prolog

$ ./hello
Hello, world!
```

</figure></details>

## Links

- [Source Code](https://github.com/extollers/extol)
- [Continuous Integration](https://hydra.atnnn.com/jobset/extol/extol#tabs-jobs)
- [Reddit](https://www.reddit.com/r/extollers/)
- [~Wikipedia~](https://en.wikipedia.org/wiki/Extol_(programming_language))
- [~Rosetta Code~](https://rosettacode.org/wiki/Category:Extol)
- [~Stack Overflow~](https://stackoverflow.com/questions/tagged/extol)
- [~Trending on GitHub~](https://github.com/trending/extol)
- [~Extol fiddle~](#)
- [License](LICENSE.md)
- [Code of Conduct](CODE_OF_CONDUCT.md)

## Inspiration

Extol is inspired in part by the following languages:

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

- [Swig](http://www.swig.org/):
  Automatically generated glue code for interfacing with other languages

- [Red](https://www.red-lang.org/p/about.html):
  Multi-paradigm, multi-typing, full-stack, human-friendy language. Fun guaranteed.

---

<details><summary>

## Roadmap

</summary><figure>

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
- [x] Integration tests
- [x] Modules
- [x] Explicit import of external functions
- [x] Don't use Prolog's eval for the REPL
- [ ] Interpreter can run the compiler
- [ ] Namespaces
- [ ] Indentation-sensitive syntax (get rid of those parentheses)
- [ ] Proper AST instead of raw terms (to allow better type checking)
- [ ] `nondet` clause annotations (to improve performance and reasoning)
- [ ] Replace `,` with `do` blocks
- [ ] Add `where` clauses
- [ ] Anonymous functions, lambdas and closures
- [ ] First-class functions
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

</figure></details>
