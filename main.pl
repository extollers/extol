#!/home/atnnn/gprolog/bin/gprolog --consult-file

%% Debugging

:- op(999, fx, tc).

% Trace
t(X) :- write('trace: '), ti, writeq(X), nl.
t(X, A, A) :-
    write('trace: '), ti, writeq(X), write(', at: '),
    copy_term(A, AA),
    (length(B, 10), append(B, _, AA) ; B = AA), !,
    prep_chars(B, Q, []),
    atom_codes(C, Q), write(C),
    length(Q, N), (N < 10 -> write('<eof>') ; true),
    nl, ! .

prep_chars([]) --> [].
prep_chars([X | Xs]) --> prep_char(X), prep_chars(Xs).

prep_char(X) --> { var(X) }, !, "<?>".
prep_char(0'\n) --> !, "<nl>".
prep_char(0'\r) --> !, "<cr>".
prep_char(0'\t) --> !, "<tab>".
prep_char(0'<) --> !, "<lt>".
prep_char(X) -->
    { ( \+integer(X) ; X < 32 ; X > 126), !,
      open_output_codes_stream(S),
      write(S, X),
      close_output_codes_stream(S, C) },
    "<", dcg_call(C), ">", !.
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

tc(F, X) -->
    undo(t(failed(F))),
    t(enter(F)),
    ticall(F, X),
    undo(t(redo(F))),
    t(exit(F)).

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

ticall(G, X, A, B) :-
    g_read(tindent, I),
    II is I + 1,
    g_assignb(tindent, II),
    G =.. L,
    append(L, [X], LL),
    GX =.. LL,
    dcg_call(GX, A, B),
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
    current_prolog_flag(argv, [_ | Args]),
    undo(halt),
    write('Running tests'), nl,
    test((Name :- Goals)),
    ([Name] = Args ; Args = []),
    write(Name), write('...'),
    once(run_test(Goals)),
    fail.

run_test(done) :- !, write(success), nl.
run_test((A, B)) :- !,
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

many(P, [X | Xs]) --> call(P, X), many(P, Xs), !.
many(_, []) --> [].

many1(P, [X | Xs]) --> call(P, X), !, many(P, Xs).

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
    many1(digit, Ds), !,
    { foldl(add_digit, 0, Ds, N) }.

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

pl_token(N) --> dcg_call(N), pl_skipwhite, !.

test pl_token :-
    pl_token("x", "x  ", ""),
    pl_token("x", "x % comment", ""),
    pl_token("x", "x % comment\n  \t", "").

pl_skipwhite --> pl_white, !.
pl_skipwhite --> [].

test pl_skipwhite :-
    pl_skipwhite("", "").

pl_white --> "%", !, pl_line_comment_, pl_skipwhite.
pl_white --> (" "; "\t"; "\r" ; "\n" ), !, pl_skipwhite.

test pl_white :-
    pl_white(" ", ""),
    pl_white("%", ""),
    pl_white("% comment \n\t  ", "").

pl_line_comment_ --> ("\n" ; eof), !.
pl_line_comment_ --> [_], pl_line_comment_.

pl_top_level(Decls) -->
    ( "#!", !, pl_line_comment_; true),
    pl_skipwhite,
    many(pl_declaration, Decls).

pl_declaration(Decl) --> pl_expression(Decl), pl_token(".").

pl_atom_char(C) -->
    [C], !,
    { member(C, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789") }.

pl_atom(Atom) -->
    "'",
    pl_quoted_atom_chars_(Cs),
    { atom_codes(Atom, Cs) }.
pl_atom(Atom) -->
    many1(pl_atom_char, Cs), !,
    { atom_codes(Atom, Cs) }.

test pl_atom :-
    pl_atom(a, "'a'", ""),
    pl_atom('+', "'+'", ""),
    pl_atom('9', "'\\9'", ""),
    pl_atom(ab, "ab", "").

pl_quoted_atom_chars_([]) --> "'", !.
pl_quoted_atom_chars_([C | Cs]) -->
    "\\", !, [Quoted],
    { member(Quoted : C, [
               0'n : 10,
               0'r : 13,
               0't : 9,
               0'e : 127,
               AsIs : AsIs
    ]) }, !,
    pl_quoted_atom_chars_(Cs).
pl_quoted_atom_chars_([C | Cs]) -->
    [C],
    pl_quoted_atom_chars_(Cs).

pl_expression(Expr) -->
    pl_expression(1201, Expr).

pl_expression(Prec, Expr) -->
    tc pl_expression(none, Prec, Expr).

test pl_expression :-
    pl_expression(1, "1", []),
    pl_expression(a, "a", []),
    pl_expression(a + b, "a + b", []),
    pl_expression(a + (b * c), "a + b * c", []),
    pl_expression((a * b) + c, "a * b + c", []),
    pl_expression((-a) * b, "-a * b", []),
    pl_expression((:- (a * b)), ":- a * b", []).

test comma_expr :-
    pl_expression((p :- (a, b)), "p :- a, b", []).

pl_regular_term(Integer) -->
    many1(digit, Ds), !,
    { foldl(add_digit, 0, Ds, Integer) },
    pl_skipwhite.
pl_regular_term(Term) -->
    pl_atom(Atom), !,
    ( pl_token("("), !,
      pl_comma_separated(Args),
      pl_token(")"),
      { Term =.. [Atom | Args] }
    ; pl_skipwhite,
      { Term = Atom }).
pl_regular_term(Term) -->
    pl_token("("),
    tc pl_expression(Term),
    pl_token(")").
pl_regular_term('{}'(Term)) -->
    pl_token("{"),
    tc pl_expression(Term),
    pl_token("}").
pl_regular_term(Term) -->
    pl_token("["),
    pl_comma_separated(Heads),
    ( pl_token("|"),
      tc pl_expression(Tail),
      { append(Heads, Tail, Term) }
    ; { Term = Heads } ),
    pl_token("]").

test pl_regular_term :-
    pl_regular_term(123, "123", ""),
    pl_regular_term(hi, "hi", ""),
    pl_regular_term(hi(1), "hi(1)", ""),
    pl_regular_term(hi(b, 4), "hi(b, 4)", ""),
    pl_regular_term(6, "(6)", ""),
    pl_regular_term('{}'(x), "{x}", ""),
    pl_regular_term([], "[]", ""),
    pl_regular_term([1,2,3], "[1,2,3]", "").

pl_comma_separated([A | As]) -->
    tc pl_expression(1000, A), !,
    ( tc pl_token(","), !,
      tc pl_comma_separated(As)
    ; { As = [] }).
pl_comma_separated([]) --> [].

pl_op_or_term(X) --> pl_regular_term(X), !.
pl_op_or_term(X) -->
    many1(pl_op_char, Cs), !,
    pl_skipwhite,
    { atom_codes(X, Cs) }, !.

pl_op_char(C) -->
    [C], { member(C, "`~!@#$%^&*<>?/;:-_=+,|") }, !.

pl_expression(none, Prec, Term) -->
    t(fz),
    tc pl_op_or_term(Op),
    { tc atom(Op),
      tc pl_op(OpPrec, Assoc, Op),
      tc member(Assoc-N, [fx-0, fy-1]),
      !,
      RightPrec is OpPrec + N },
    tc pl_expression(none, RightPrec, Right),
    { Combined =.. [Op, Right] },
    tc pl_expression(just(Combined), Prec, Term).
pl_expression(none, Prec, Term) --> !,
    t(z),
    tc pl_op_or_term(Left), !,
    tc pl_expression(just(Left), Prec, Term).
pl_expression(just(Left), Prec, Term) -->
    t(zf),
    tc pl_op_or_term(Op),
    { atom(Op),
      pl_op(OpPrec, Assoc, Op),
      member(Assoc-N, [xf-0, yf-1]),
      LeftPrec is OpPrec + N,
      tc LeftPrec < Prec,
      !,
      Combined =.. [Op, Left] },
    tc pl_expression(just(Combined), Prec, Term).
pl_expression(just(Left), Prec, Term) -->
    t(zfz),
    tc pl_op_or_term(Op),
    { atom(Op),
      pl_op(OpPrec, Assoc, Op),
      member(Assoc-N-M, [xfx-0-0, xfy-0-1, yfx-1-0]),
      LeftPrec is OpPrec + N,
      tc LeftPrec < Prec,
      !,
      RightPrec is OpPrec + M },
    tc pl_expression(none, RightPrec, Right),
    { Combined =.. [Op, Left, Right] },
    tc pl_expression(just(Combined), Prec, Term).
pl_expression(just(Term), _, Term) -->
    !,
    t(just(z)).

pl_op(1200, xfx, ':-').
pl_op(1200, xfx, '-->').
pl_op(1200, fx, ':-').
pl_op(1105, xfy, '|').
pl_op(1100, xfy, ';').
pl_op(1050, xfy, '->').
pl_op(1000, xfy, ',').
pl_op(900, fy, '\\+').
pl_op(700, xfx, '=').
pl_op(700, xfx, '\\=').
pl_op(700, xfx, '=..').
pl_op(700, xfx, '==').
pl_op(700, xfx, '\\==').
pl_op(700, xfx, 'is').
pl_op(700, xfx, '<').
pl_op(700, xfx, '>').
pl_op(700, xfx, '=<').
pl_op(700, xfx, '>=').
pl_op(700, xfx, '=\\=').
pl_op(600, xfy, ':').
pl_op(500, yfx, '+').
pl_op(500, yfx, '-').
pl_op(400, yfx, '*').
pl_op(400, yfx, '/').
pl_op(400, yfx, 'rem').
pl_op(400, yfx, 'mod').
pl_op(400, yfx, 'div').
pl_op(400, yfx, '<<').
pl_op(400, yfx, '>>').
pl_op(200, xfx, '**').
pl_op(200, xfx, '^').
pl_op(200, fy, '+').
pl_op(200, fy, '-').

%test parse_self :-
%    read_file('main.pl', Bytes), !,
%    pl_top_level(_Decls, Bytes, []).

test wip :-
    %tc pl_expression(just(b), 1000 , _, ",", _).
    tc pl_regular_term(_, "hi(b, 4)", "").

