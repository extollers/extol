% Generated by extoltoprolog
:-(set_prolog_flag(singleton_warning,off)).
:-(discontiguous(/(test,1))).
:-(initialization(;(main,','(write('error: unexpected failure in main'),','(nl,halt(1)))))).
:-(unify(A,A),true).
:-(+(A,B,C),','(true,','(=('.'(A,'.'(B,'.'(C,[]))),'.'(D,'.'(E,'.'(F,[])))),is(F,+(D,E))))).
:-(-(A,B,C),','(true,','(=('.'(A,'.'(B,'.'(C,[]))),'.'(D,'.'(E,'.'(F,[])))),is(F,-(D,E))))).
:-(fib(0,1),true).
:-(fib(1,1),true).
:-(fib(A,B),','(-(A,1,C),','(fib(C,D),','(-(A,2,E),','(fib(E,F),+(D,F,B)))))).
:-(main,','(','(fib(12,A),write(A)),halt)).
