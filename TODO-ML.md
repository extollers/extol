# Standard ML Backend Roadmap

Tasks required to achieve full feature parity with the Prolog backend and enable self-hosting via MLton.

## Runtime (`src/runtime/runtime.sml`)
- [ ] **Nondeterminism & Backtracking**: Fully implement the choice-point system using the existing `Stack.Frame` and `MLton.Cont`.
- [ ] **Advanced Unification**: 
    - [ ] Proper handling of `Unknown` (holes) in all Value types.
    - [ ] Occurs check (if needed).
    - [ ] Symbol/Atom interning (currently uses `word`).
- [ ] **Garbage Collection Integration**: Ensure `Hole` references don't leak or cause issues with MLton's GC.
- [ ] **Primitive Types**:
    - [ ] Real numbers.
    - [ ] Symbols (Atoms).
    - [ ] Records (extensible).
    - [ ] Streams/IO handles.

## Compiler (`src/extol/compile-sml.xtl`)
- [ ] **Pattern Matching**:
    - [ ] Generate SML match expressions or unification calls for multiple clauses.
    - [ ] Support complex nested patterns in function heads.
- [ ] **Control Flow**:
    - [ ] Disjunction (`A ; B`).
    - [ ] Cut (`!`).
    - [ ] If-Then-Else (`A -> B ; C`).
    - [ ] Soft cut / negation.
- [ ] **Backtracking Code Gen**:
    - [ ] Transform predicates into continuation-passing style (CPS) or use the runtime's try-stack for nondeterministic predicates.
- [ ] **DCG Support**:
    - [ ] Implement `dcg` and `dcg2` transformations.
- [ ] **Annotations & Contracts**:
    - [ ] Support `requires`, `ensures`, `parameters`, and `inline`.
- [ ] **Module System**:
    - [ ] Handle imports and qualified names correctly.
- [ ] **Foreign Interface**:
    - [ ] Provide a way to call arbitrary SML code (similar to `prolog$`).

## Prelude & Builtins
- [ ] **Math**: Implement all arithmetic operators.
- [ ] **List Processing**: Port `append`, `member`, `maplist`, etc., or ensure they compile correctly from the prelude.
- [ ] **I/O**: Implement `open`, `close`, `get_byte`, `put_byte`.
- [ ] **Meta-programming**: Implement `copy_term`, `numbervars`, `read_file`, `write_file`.

## Infrastructure & Testing
- [ ] **Self-Hosting**: Reach the point where `build/stage-ml` can compile `src/main.xtl`.
- [ ] **Test Coverage**:
    - [ ] Migrate `test/eval` to run against the SML backend.
    - [ ] Add performance benchmarks comparing Prolog vs SML execution.
- [ ] **Optimization**:
    - [ ] Tail-call optimization for deterministic predicates.
    - [ ] Indexing for clause selection.
