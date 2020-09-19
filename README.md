# Extol

Extol is a programming language being bootstrapped on top of Prolog.

The language is incomplete and the syntax and semantics are still
closely tied to Prolog.

## Documentation

<details><summary><b>Examples</b></summary><figure>

Extol will eventually check types at compile time with declarations
that may look like this:

```
define fib :: number -> number
  0: 1
  1: 1
  n: fib (n - 1) + fib(n - 2)
```

However, the types are currently only checked at runtime and
declarations look like this:

```
define fib:
    arguments(N),
    returns X,
    requires number(N),
    ensures number(X),

    (0: 1),
    (1: 1),
    (N: fib(N - 1) + fib(N - 2)).
```

Arbitrary Prolog-style predicates can be used as types, such as these
types used by the compiler:

```
pred declaration:
    (define(Name, Annots, Clauses):
        atom(Name),
	maplist(annotation, Annots),
	maplist(define_clause, Clauses)),
    (test(Name, Goals):
        atom(Name),
	goal(Goals)).

pred annotation:
    (nondet: true),
    (predicate: true),
    (returns(Var): true),
    (ensures(Pred): xtl_goal(Pred)),
    (requires(Pred): xtl_goal(Pred)),
    (traced: true),
    (dcg: true),
    (parameters(List): maplist(var, List)).
```

Extol features declarative grammars:

```
dcg xtl_regular_term:
((Char) : "0'", !, require(xtl_string_char(Char)), xtl_skipwhite),
((Integer) :
    many1(digit, Ds), !,
    { foldl(add_digit, 0, Ds, Integer), ! },
    xtl_skipwhite),
((String) : "\"", !, require(many(xtl_string_char, String)), require("\""), xtl_skipwhite),
((Term) :
    xtl_atom(Atom), !,
    ( xtl_token("("), !,
      xtl_comma_separated(Args, [], xtl_token(")")),
      { Term =... [Atom | Args] }
    ; xtl_skipwhite,
      { Term = Atom })),
% ...
```

With a built-in test framework that allows writing tests like this:

```
test xtl_regular_term :
    xtl_regular_term(123, "123", ""),
    xtl_regular_term(hi, "hi", ""),
    xtl_regular_term(hi(1), "hi(1)", ""),
    xtl_regular_term(hi(b, 4), "hi(b, 4)", ""),
    % ...
```

</figure></details>

<details><summary><b>Quick Start</b></summary><figure>

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
nix-shell
```

Build the compiler and install it to `./local`:

```
make install
```

#### Using the REPL

```
$ ./local/bin/extol repl

Extol> 1 + 1
2

Extol> 'Hello, world!'
Hello, world!
```

### Using the compiler

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
- [Repl.it](https://repl.it/github/extollers/extol)
- [Reddit](https://www.reddit.com/r/extollers/)
- [~Wikipedia~](https://en.wikipedia.org/wiki/Extol_(programming_language))
- [~Rosetta Code~](https://rosettacode.org/wiki/Category:Extol)
- [~Stack Overflow~](https://stackoverflow.com/questions/tagged/extol)
- [~Trending on GitHub~](https://github.com/trending/extol)
- [~rise4fun~](https://rise4fun.com/Extol)
- [License](LICENSE.md)
- [Code of Conduct](CODE_OF_CONDUCT.md)

## Inspiration

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

---

<details><summary><b>Roadmap</b></summary><figure>

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
- [ ] Modules and namespaces
- [ ] Explicit import of external functions
- [ ] Don't use Prolog's eval for the REPL
- [ ] Indentation-sensitive syntax (get rid of those parentheses)
- [ ] Proper AST instead of raw terms (to allow better type checking)
- [ ] `nondet` clause annotations (to improve performance and reasoning)
- [ ] Replace `,` with `do` blocks
- [ ] Add `where` clauses
- [ ] Static type checking
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