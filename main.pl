#!/home/atnnn/gprolog/bin/gprolog --consult-file

%% Debugging

% Trace
t(X) :- write('trace: '), ti, write(X), nl.
t(X, A, A) :-
    write('trace: '), ti, write(X), write(', at: '),
    copy_term(A, AA),
    (length(B, 10), append(B, _, AA) ; B = AA),
    prep_chars(B, Q, []), atom_codes(C, Q), write(C),
    length(Q, N), N < 10 -> write('<eof>'),
    nl, ! .

prep_chars([]) --> [].
prep_chars([X | Xs]) --> prep_char(X), prep_chars(Xs).

prep_char(X) --> { var(X) }, !, "<?>".
prep_char(0'\n) --> !, "<nl>".
prep_char(0'\r) --> !, "<cr>".
prep_char(0'\t) --> !, "<tab>".
prep_char(0'<) --> !, "<lt>".
prep_char(X) --> [X].

% Trace and fail
tf(X) :- t(X), fail.
tf(X, A, B) :- tf(X, A, B), fail.

% Trace call
tc(F) :-
    undo(t(failed(F))),
    t(enter(F)),
    ticall(F),
    undo(t(redo(F))),
    t(exit(F)).

tc(F) -->
    undo(t(failed(F))),
    t(enter(F)),
    ticall(F),
    undo(t(redo(F))),
    t(exit(F)).

:- op(999, fx, tc).

% Trace indent

ti :- g_read(tindent, I), II is I * 2, length(L, II), maplist('='(0' ), L), atom_codes(S, L), write(S).

ticall(G) :-
    g_read(tindent, I),
    II is I + 1,
    g_assignb(tindent, II),
    call(G),
    g_assignb(tindent, I).

ticall(G, A, B) :-
    g_read(tindent, I),
    II is I + 1,
    g_assignb(tindent, II),
    dcg_call(G, A, B),
    g_assignb(tindent, I).

% Call on undo

undo(_).
undo(G) :- call(G), fail.

undo(_, A, A).
undo(G, A, B) :- call(G, A, B), fail.

%% Streams

read_bytes(Stream, []) :- at_end_of_stream(Stream), !.
read_bytes(Stream, [X | Xs]) :-
    get_byte(Stream, X),
    read_bytes(Stream, Xs).

read_file(Path, Bytes) :-
    open(Path, read, Stream, [type(binary), buffering(block)]),
    read_bytes(Stream, Bytes),
    close(Stream).

%% Main

:- initialization(main).

main :-
    undo(halt),
    write('Running tests'), nl,
    test((Name :- Goals)),
    write(Name), write('...'),
    once(run_test(Goals)),
    fail.

once(G) :- call(G), !.

run_test(done) :- write(success), nl.
run_test((A, B)) :-
    call(A)
    -> run_test(B)
    ; nl, write('  failed: '), write(A), nl.
run_test(B) :-
    run_test((B, done)).

:- op(1200, fy, test).
:- discontiguous('/'(test, 1)).

test 'parse test.c' :-
    read_file('test.c', Bytes), !,
    c_pp([], Tokens, Bytes, []), !,
    c_top_level(_Decls, Tokens, []).

%% Parsing

many(P, [X | Xs], R, RRR) :- call(P, X, R, RR), many(P, Xs, RR, RRR).
many(_, [], R, R).

eof([], []).

peek(Rest, Rest, Rest).

push(X, Rest, [X | Rest]).

alpha(C) --> [C], { member(C, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") }.

digit(D) -->
    [C],
    { member(C - D, [0'0 - 0, 0'1 - 1, 0'2 - 2, 0'3 - 3, 0'4 - 4, 0'5 - 5, 0'6 - 6, 0'7 - 7, 0'8 - 8, 0'9 - 9]) }.

dcg_call([]) --> !, [].
dcg_call([X | Xs]) --> !, [X], dcg_call(Xs).
dcg_call((A, B)) --> !, dcg_call(A), dcg_call(B).
dcg_call((A ; B)) --> !, dcg_call(A); dcg_call(B).
dcg_call(G, In, Rest) :- !, call(G, In, Rest).

%% Lists

foldl(_, R, [], R).
foldl(F, Z, [X | Xs], RR) :- call(F, Z, X, R), foldl(F, R, Xs, RR).

%% C preprocessor

c_pp(Env, Tokens) -->
    c_pp_lines(Lines), eof, !,
    { c_pp_eval(Env, Lines, Tokens, []) }.

c_pp_lines([]) --> eof, !.
c_pp_lines([Line | Lines]) --> c_pp_line(Line), !, c_pp_lines(Lines).

c_pp_line([]) --> c_pp_skipwhite, ("\n", ! ; eof), !.
c_pp_line([Token | Tokens]) --> c_pp_skipwhite, !, c_pp_token(Token), !, c_pp_line(Tokens).

c_pp_skipwhite --> c_pp_white, !.
c_pp_skipwhite --> [].

c_pp_white --> "//", !, c_pp_line_comment_.
c_pp_white --> "/*", !, c_pp_block_comment_, c_pp_skipwhite.
c_pp_white --> (" "; "\t"; "\r" ; "\\\r\n" ; "\\\n"), !, c_pp_skipwhite.

c_pp_line_comment_ --> (peek([0'\n | _]) ; eof), !.
c_pp_line_comment_ --> c_pp_white, !, c_pp_line_comment_.
c_pp_line_comment_ --> [_], !, c_pp_line_comment_.

c_pp_block_comment_ --> "*/", !.
c_pp_block_comment_ --> [_], c_pp_block_comment_.

c_pp_token(Token) --> (c_pp_operator(Token) ; c_pp_symbol(Token) ; c_pp_integer(Token)), !.

c_pp_operator(operator(Token), Next, Rest) :-
    member(Op, [
               "=",
               "#", ";"
    ]),
    append(Op, Rest, Next),
    atom_codes(Token, Op).

c_pp_symbol(symbol(Name)) --> c_pp_symbol_chars(Cs), { atom_codes(Name, Cs) }.

c_pp_symbol_chars([X | Xs]) --> c_pp_symbol_first(X), !, many(c_pp_symbol_char, Xs).

c_pp_symbol_first(C) --> alpha(C), !.
c_pp_symbol_first(0'_) --> "_".

c_pp_symbol_char(C) --> c_pp_symbol_first(C); [C], { member(C, "0123456789") }.

add_digit(N, D, R) :-
    member(D, [0,1,2,3,4,5,6,7,8,9]),
    ( var(N), N is R div D; true),
    R is N * 10 + D.

c_pp_integer(integer(N)) -->
    many(digit, Ds), !,
    { Ds = [_|_],
      foldl(add_digit, 0, Ds, N) }.

c_pp_eval(_, []) --> eof, !.
c_pp_eval(EnvA, [Line | Lines]) -->
    c_pp_eval_line(EnvA, EnvB, Line), !,
    c_pp_eval(EnvB, Lines).

c_pp_eval_line(Env, [Name = Value | Env], [operator('#'), symbol('define'), symbol(Name) | Value]) --> !.
c_pp_eval_line(Env, Env, []) --> !.
c_pp_eval_line(Env, Env, [symbol(X) | Xs]) -->
    { member(X = Ys, Env), !, append(Ys, Xs, Zs) },
    c_pp_eval_line(Env, Env, Zs).
c_pp_eval_line(Env, Env, [X | Xs]) -->
    [X],
    c_pp_eval_line(Env, Env, Xs).

%% C

c_top_level(Decls) --> many(c_declaration, Decls), eof.

c_declaration(declare(Name, Type, Value)) -->
    c_type(Type), [symbol(Name)],
    ( [operator(=)], c_value(Assign), { Value = value(Assign) }
    ; { Value = none } ),
    [operator(;)].

c_type(Type) --> [symbol(Type)].

c_value(variable(Name)) --> [symbol(Name)].
c_value(integer(N)) --> [integer(N)].

%% Extol

e_token(N) --> dcg_call(N), e_skipwhite, !.

test e_token :-
    e_token("x", "x  ", ""),
    e_token("x", "x # comment", ""),
    e_token("x", "x # comment\n  \t", "").

e_skipwhite --> e_white, !.
e_skipwhite --> [].

test e_skipwhite :-
    e_skipwhite("", "").

e_white --> "#", !, e_line_comment_, e_skipwhite.
e_white --> (" "; "\t"; "\r" ; "\n" ), !, e_skipwhite.

test e_white :-
    e_white(" ", ""),
    e_white("#", ""),
    e_white("# comment \n\t  ", "").

e_line_comment_ --> ("\n" ; eof), !.
e_line_comment_ --> [_], e_line_comment_.

e_top_level(Decls) --> many(e_declaration, Decls).

e_declaration(Decl) --> e_expression(Decl), e_token(".").

e_atom_char(C) -->
    [C], !,
    { member(C, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789") }.

e_atom(Atom) -->
    "'",
    e_quoted_atom_chars_(Cs),
    { atom_codes(Atom, Cs) }.
e_atom(Atom) -->
    many(e_atom_char, Cs), !,
    { atom_codes(Atom, Cs) }.

test e_atom :-
    e_atom(a, "'a'", ""),
    e_atom('+', "'+'", ""),
    e_atom('9', "'\\9'", ""),
    e_atom(ab, "ab", "").

e_quoted_atom_chars_([]) --> "'", !.
e_quoted_atom_chars_([C | Cs]) -->
    "\\", !, [Quoted],
    { member(Quoted : C, [
               0'n : 10,
               0'r : 13,
               0't : 9,
               0'e : 127,
               AsIs : AsIs
    ]) }, !,
    e_quoted_atom_chars_(Cs).
e_quoted_atom_chars_([C | Cs]) -->
    [C],
    e_quoted_atom_chars_(Cs).

e_expression(Expr) -->
    many(e_op_or_term, Flat),
    e_apply_ops(Flat, Expr).

e_regular_term(Integer) -->
    tc many(digit, Ds),
    { Ds = [_|_], !,
      tc foldl(add_digit, 0, Ds, Integer) },
    tc e_skipwhite.
e_regular_term(Term) -->
    tc e_atom(Atom),
    ( tc e_token("("), !,
      tc e_comma_separated(Args),
      tc e_token(")"),
      { Term =.. [Atom | Args] }
    ; { Term = Atom }).
e_regular_term(Term) -->
    e_token("("),
    e_expression(Term),
    e_token(")").
e_regular_term('{}'(Term)) -->
    e_token("{"),
    e_expression(Term),
    e_token("}").
e_regular_term(Term) -->
    e_token("["),
    e_comma_separated(Heads),
    ( e_token("|"),
      e_expression(Tail),
      { append(Heads, Tail, Term) }
    ; { Term = Heads } ),
    e_token("]").

test e_regular_term :-
    e_regular_term(123, "123", ""),
    tc e_regular_term(hi, "hi", ""),
    e_regular_term(hi(1), "hi(1)", ""),
    e_regular_term(hi(b, 4), "hi(b, 4)", ""),
    e_regular_term(6, "(6)", ""),
    e_regular_term('{}'(x), "{x}", ""),
    e_regular_term([], "[]", ""),
    e_regular_term([1,2,3], "[1,2,3]", "").


e_comma_separated([A | As]) -->
    e_expression(A), !,
    e_token(","),
    e_comma_separated(As).
e_comma_separated([]).

e_op_or_term(X) --> e_regular_term(X), !.
e_op_or_term(X) -->
    e_token(e_op_chars(Cs)),
    atom_codes(X, Cs).

e_op_chars([C | Cs]) -->
    [C], { member(C, "`~!@#$%^&*,<>?/;:-_=+") }, !,
    e_op_chars(Cs).

e_apply_ops(Flat, Term) :- e_apply_ops(Term, Flat, []).

e_apply_ops(_, Term) --> [term(Term)], !.
e_apply_ops(Prec, Term) -->
    [Op], atom(Op),
    e_op(NewPrec, Assoc, Op),
    NewPrec =< Prec,
    member(Assoc-N, [fx-1, fy-0]),
    !,
    RightPrec is NewPrec - N,
    e_apply_op(RightPrec, Right),
    Combined =.. [Op, Right],
    push(Combined),
    e_apply_ops(Prec, Term).
e_apply_ops(Prec, Term) -->
    [Left, Op], atom(Op),
    e_op(NewPrec, Assoc, Op),
    member(Assoc-N, [xf-1, yf-0]),
    LeftPrec is NewPrec - N,
    LeftPrec =< Prec,
    !,
    Combined =.. [Op, Left],
    push(Combined),
    e_apply_ops(Prec, Term).
e_apply_ops(Prec, Term) -->
    [Left, Op], atom(Op),
    e_op(NewPrec, Assoc, Op),
    member(Assoc-N-M, [xfx-1-1, xfy-1-0, yfx-0-1]),
    LeftPrec is NewPrec - N,
    LeftPrec =< Prec,
    !,
    RightPrec is NewPrec - M,
    e_apply_op(RightPrec, Right),
    Combined =.. [Op, Left, Right],
    push(Combined),
    e_apply_ops(Prec, Term).
