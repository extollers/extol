% Generated by extoltoprolog
:-(set_prolog_flag(singleton_warning,off)).
:-(discontiguous(/(test,1))).
:-(initialization(;(main,','(write('error: unexpected failure in main'),','(nl,halt(1)))))).
:-(log(A),true).
:-(t(A),','(write('trace: '),','(ti,','(write_term(A,'.'(quoted(true),'.'(max_depth(7),[]))),nl)))).
:-(t(A,B,C),','(','(','(write('trace: '),','(ti,','(write_term(A,'.'(quoted(true),'.'(max_depth(7),[]))),write(', at: ')))),=(B,D)),','(peek(E,D,F),','(','(','(pretty_init(E,G),','(write(G),nl)),=(F,H)),','(!,=(H,C)))))).
:-(pretty_init(A,B),','(copy_term(A,C),','(;(','(length(D,32),append(D,E,C)),=(D,C)),','(!,','(prep_chars(D,F,[]),','(length(F,G),','(;(->(<(G,10),append(F,'.'(60,'.'(101,'.'(111,'.'(102,'.'(62,[]))))),H)),=(H,F)),atom_codes(B,H)))))))).
:-(prep_chars([],A,B),=(A,B)).
:-(prep_chars('.'(A,B),C,D),','(prep_char(A,C,E),prep_chars(B,E,D))).
:-(prep_char(A,B,C),','(','(var(A),=(B,D)),','(','(!,=(D,E)),append('.'(60,'.'(63,'.'(62,[]))),C,E)))).
:-(prep_char(10,A,B),','(','(!,=(A,C)),append('.'(60,'.'(110,'.'(108,'.'(62,[])))),B,C))).
:-(prep_char(13,A,B),','(','(!,=(A,C)),append('.'(60,'.'(99,'.'(114,'.'(62,[])))),B,C))).
:-(prep_char(9,A,B),','(','(!,=(A,C)),append('.'(60,'.'(116,'.'(97,'.'(98,'.'(62,[]))))),B,C))).
:-(prep_char(60,A,B),','(','(!,=(A,C)),append('.'(60,'.'(108,'.'(116,'.'(62,[])))),B,C))).
:-(prep_char(A,B,C),','(','(','(;(\+(integer(A)),;(<(A,32),>(A,126))),','(!,','(open_output_codes_stream(D),','(write(D,A),close_output_codes_stream(D,E))))),=(B,F)),','(append('.'(60,[]),G,F),','(dcg_call(E,G,H),','(append('.'(62,[]),I,H),','(!,=(I,C))))))).
:-(prep_char(A,B,C),append('.'(A,[]),C,B)).
:-(tf(A),','(t(A),fail)).
:-(tf(A,B,C),','(tf(A,B,C),fail)).
:-(tc(A),','(undo(t(failed(A))),','(t(enter(A)),','(ticall(A),','(undo(t(redo(A))),t(exit(A))))))).
:-(tc(A,B,C),','(undo(t(failed(A)),B,D),','(t(enter(A),D,E),','(ticall(A,E,F),','(undo(t(redo(A)),F,G),t(exit(A),G,C)))))).
:-(tc(A,B,C,D),','(undo(t(failed(A)),C,E),','(t(enter(A),E,F),','(ticall(A,B,F,G),','(undo(t(redo(A)),G,H),t(exit(A),H,D)))))).
:-(ti,','(=(A,'.'(124,'.'(32,'.'(46,'.'(32,'.'(46,'.'(32,A))))))),','(g_read(tindent,B),','(is(C,*(B,2)),','(length(D,C),','(append(D,E,A),','(atom_codes(F,D),write(F)))))))).
:-(ticall(A),','(g_read(tindent,B),','(is(C,+(B,1)),','(g_assignb(tindent,C),','(call(A),g_assignb(tindent,B)))))).
:-(ticall(A,B,C),','(g_read(tindent,D),','(is(E,+(D,1)),','(g_assignb(tindent,E),','(dcg_call(A,B,C),g_assignb(tindent,D)))))).
:-(ticall(A,B,C,D),','(g_read(tindent,E),','(is(F,+(E,1)),','(g_assignb(tindent,F),','(=...(A,G),','(append(G,'.'(B,[]),H),','(=...(I,H),','(dcg_call(I,C,D),g_assignb(tindent,E))))))))).
:-(write_trace_line(A),','(write('at: '),','(write_term(A,'.'(numbervars(false),'.'(namevars(false),'.'(max_depth(6),[])))),nl))).
:-(throw_with_trace(traced_exception(A,B),C),','(!,throw(traced_exception(A,'.'(C,B))))).
:-(throw_with_trace(A,B),throw(traced_exception(A,'.'(B,[])))).
:-(print_exception(traced_exception(A,B)),','(!,','(print_exception(A),','(reverse(B,C),maplist(write_trace_line,C))))).
:-(print_exception(A),','(write_term(A,'.'(numbervars(false),'.'(namevars(false),'.'(max_depth(6),[])))),nl)).
:-(sf(A),catch(A,B,throw_with_trace(B,A))).
:-(sf(A,B,C),sf(call(A,B,C))).
:-(test_sf1,sf(test_sf2)).
:-(test_sf2,throw(boop)).
test(:-(trace,','(catch(sf(test_sf1),A,true),=(A,traced_exception(boop,'.'(test_sf1,'.'(test_sf2,[]))))))).
:-(many(A,'.'(B,C),D,E),','(call(A,B,D,F),','(many(A,C,F,G),','(!,=(G,E))))).
:-(many(A,[],B,C),=(B,C)).
:-(many(A,B,C),','(call(A,B,D),','(many(A,D,E),','(!,=(E,C))))).
:-(many(A,B,C),','(true,=(B,C))).
:-(many1(A,B,C,D),','(assert(','(callable(A),list_or_partial_list(B))),','('__contract_free_many1'(A,B,C,D),assert(list(B))))).
:-('__contract_free_many1'(A,'.'(B,C),D,E),','(call(A,B,D,F),','(','(!,=(F,G)),many(A,C,G,E)))).
:-(eof(A,B),peek([],A,B)).
:-(peek(A,B,C),','(assert(','(list_or_partial_list(B),','(list_or_partial_list(C),list_or_partial_list(A)))),','('__contract_free_peek'(A,B,C),assert(true)))).
:-('__contract_free_peek'(A,A,A),true).
:-(alpha(A,B,C),','(assert(true),','('__contract_free_alpha'(A,B,C),assert(number(A))))).
:-('__contract_free_alpha'(A,B,C),','(append('.'(A,[]),D,B),','(member(A,'.'(97,'.'(98,'.'(99,'.'(100,'.'(101,'.'(102,'.'(103,'.'(104,'.'(105,'.'(106,'.'(107,'.'(108,'.'(109,'.'(110,'.'(111,'.'(112,'.'(113,'.'(114,'.'(115,'.'(116,'.'(117,'.'(118,'.'(119,'.'(120,'.'(121,'.'(122,'.'(65,'.'(66,'.'(67,'.'(68,'.'(69,'.'(70,'.'(71,'.'(72,'.'(73,'.'(74,'.'(75,'.'(76,'.'(77,'.'(78,'.'(79,'.'(80,'.'(81,'.'(82,'.'(83,'.'(84,'.'(85,'.'(86,'.'(87,'.'(88,'.'(89,'.'(90,[]))))))))))))))))))))))))))))))))))))))))))))))))))))),=(D,C)))).
:-(digit(A,B,C),','(assert(true),','('__contract_free_digit'(A,B,C),assert(number(A))))).
:-('__contract_free_digit'(A,B,C),','(append('.'(D,[]),E,B),','(member(-(D,A),'.'(-(48,0),'.'(-(49,1),'.'(-(50,2),'.'(-(51,3),'.'(-(52,4),'.'(-(53,5),'.'(-(54,6),'.'(-(55,7),'.'(-(56,8),'.'(-(57,9),[]))))))))))),=(E,C)))).
:-(dcg_call(A,B,C),','(var(A),','(!,fail))).
:-(dcg_call([],A,A),!).
:-(dcg_call('.'(A,B),C,D),','(!,','(=(C,'.'(A,E)),dcg_call(B,E,D)))).
:-(dcg_call(','(A,B),C,D),','(!,','(dcg_call(A,C,E),dcg_call(B,E,D)))).
:-(dcg_call(;(A,B),C,D),','(!,;(dcg_call(A,C,D),dcg_call(B,C,D)))).
:-(dcg_call({}(A),B,B),','(!,call(A))).
:-(dcg_call(A,B,C),','(!,call(A,B,C))).
:-(require(A,B,C),;(','(dcg_call(A,B,C),!),','(pretty_init(B,D),throw(parse_failed(A,D))))).
:-(must(A),','(undo(throw(failed(A))),call(A))).
:-(must(A,B),','(undo(throw(failed(call(A,B)))),call(A,B))).
:-(must(A,B,C),','(undo(throw(failed(call(A,B,C)))),call(A,B,C))).
:-(must(A,B,C,D),','(undo(throw(failed(call(A,B,C,D)))),call(A,B,C,D))).
:-(try(A,B,C),','(catch(dcg_call(A,B,C),parse_failed(D,E),=(F,true)),','(!,=(F,false)))).
:-(add_digit(A,B,C),','(member(B,'.'(0,'.'(1,'.'(2,'.'(3,'.'(4,'.'(5,'.'(6,'.'(7,'.'(8,'.'(9,[]))))))))))),','(;(','(var(A),is(A,div(C,B))),true),is(C,+(*(A,10),B))))).
:-(undo(A),true).
:-(undo(A),','(call(A),fail)).
:-(undo(A,B,C),','(true,=(B,C))).
:-(undo(A,B,C),','(dcg_call(A,B,D),','(fail,=(D,C)))).
:-(byte(A),','(number(A),','(>=(A,0),<(A,256)))).
:-(bytes(A),','(ground(A),maplist(byte,A))).
:-(assert(','(A,B)),','(!,','(assert(A),assert(B)))).
:-(assert(maplist(A,[])),!).
:-(assert(maplist(A,'.'(B,C))),','(!,','(assert(call(A,B)),','(!,assert(maplist(A,C)))))).
:-(assert(A),;(','(call(A),!),throw(assert_failed(A)))).
:-(foldl(A,B,[],B),true).
:-(foldl(C,D,'.'(E,F),G),','(call(C,D,E,H),foldl(C,H,F,G))).
:-(append([],[]),true).
:-(append('.'(A,B),C),','(append(B,D),append(A,D,C))).
:-(comma_list(A,'.'(B,[])),','(var(A),','(!,=(B,A)))).
:-(comma_list(C,'.'(C,[])),','(var(C),!)).
:-(comma_list(','(B,D),E),','(=(E,'.'(B,F)),','(!,','(comma_list(D,F),!)))).
:-(comma_list('()',[]),true).
:-(comma_list(B,'.'(B,[])),true).
:-('.'(A,B,'.'(A,B)),true).
test(:-(comma_list,','(comma_list('()',[]),','(comma_list(1,'.'(1,[])),','(comma_list(','(1,2),'.'(1,'.'(2,[]))),','(comma_list(','(1,','(2,3)),'.'(1,'.'(2,'.'(3,[])))),','(comma_list(A,'.'(A,[])),','(comma_list(','(1,','(2,A)),'.'(1,'.'(2,'.'(A,[])))),','(comma_list(B,'.'(14,[])),=(B,14)))))))))).
:-(map_comma_list(A,B),','(comma_list(B,C),maplist(A,C))).
:-(error_unless(A,B),','(call(A),!)).
:-(error_unless(A,B),throw(error(B))).
:-(error_unless(A),error_unless(A,goal_failed(A))).
:-(=...(A,B),','(assert(','(;(compound(A),;(atom(A),','(=(B,'.'(C,D)),atom(C)))),list_or_partial_list(B))),','('__contract_free_=...'(A,B),assert(true)))).
:-('__contract_free_=...'(A,B),=..(A,B)).
:-(+(A,B,C),is(C,+(A,B))).
:-(-(A,B,C),is(C,-(A,B))).
:-(fib(0,1),true).
:-(fib(1,1),true).
:-(fib(A,B),','(-(A,1,C),','(fib(C,D),','(-(A,2,E),','(fib(E,F),+(D,F,B)))))).
:-(snd(-(A,B),B),true).
test(:-(misc,snd(-(2,1),1))).
:-(pmaplist(A,B),','(var(B),!)).
:-(pmaplist(A,[]),true).
:-(pmaplist(A,'.'(B,C)),','(call(A,B),pmaplist(A,C))).
test(:-(pmaplist,','(pmaplist(=(1),[]),','(pmaplist(=(1),'.'(1,A)),','(pmaplist(=(1),'.'(1,'.'(1,[]))),pmaplist(=(1),B)))))).
:-(pmember(A,B),','(var(B),','(!,false))).
:-(pmember(A,[]),','(!,false)).
:-(pmember(A,'.'(A,B)),','(!,true)).
:-(pmember(A,'.'(B,C)),pmember(A,C)).
:-(pinsert(A,B),','(var(B),','(!,=(B,'.'(A,C))))).
:-(pinsert(A,'.'(B,C)),pinsert(A,C)).
:-(plength(A,0),','(var(A),!)).
:-(plength([],0),true).
:-(plength('.'(B,C),D),','(plength(C,E),+(1,E,D))).
:-(filterlist(A,[],[]),true).
:-(filterlist(A,'.'(B,C),D),;(','(call(A,B),','(!,','(filterlist(A,C,E),=(D,'.'(B,E))))),filterlist(A,C,D))).
:-('()',true).
:-(read_bytes(A,B),','(assert(ground(A)),','('__contract_free_read_bytes'(A,B),assert(bytes(B))))).
:-('__contract_free_read_bytes'(A,[]),','(at_end_of_stream(A),!)).
:-('__contract_free_read_bytes'(A,'.'(B,C)),','(get_byte(A,B),'__contract_free_read_bytes'(A,C))).
:-(write_bytes(A,B),','(assert(','(ground(A),','(current_stream(A),bytes(B)))),','('__contract_free_write_bytes'(A,B),assert(true)))).
:-('__contract_free_write_bytes'(A,[]),true).
:-('__contract_free_write_bytes'(A,'.'(B,C)),','(put_byte(A,B),'__contract_free_write_bytes'(A,C))).
:-(read_file(A,B),','(assert(atom(A)),','('__contract_free_read_file'(A,B),assert(bytes(B))))).
:-('__contract_free_read_file'(A,B),','(open(A,read,C,'.'(type(binary),'.'(buffering(block),[]))),','(read_bytes(C,B),close(C)))).
:-(write_file(A,B),','(assert(','(ground(A),bytes(B))),','('__contract_free_write_file'(A,B),assert(true)))).
:-('__contract_free_write_file'(A,B),','(open(A,write,C,'.'(type(binary),'.'(buffering(block),[]))),','(write_bytes(C,B),close(C)))).
:-(read_line([]),','(current_input(A),','(at_end_of_stream(A),!))).
:-(read_line('.'(A,B)),','(current_input(C),','(line_count(C,D),','(get_char(E),','(atom_codes(E,'.'(A,[])),;(','(line_count(C,D),read_line(B)),=(B,[]))))))).
:-(xtl_include(A,B),','(log(including(A)),','(read_file(A,C),must(xtl_top_level(B,C,[]))))).
:-(xtl_import(A,B,C),','(log(importing(A,B)),','(must(:=(D,xtlm_imports(C))),','(;(','(tc(pmember(-(B,E),D)),','(!,=(F,module(B,E,D)))),','(tc(must(:=(F,xtlm_new(B,D)))),','(','(read_file(A,G),must(xtl_top_level(F,G,[]))),must(xtlm_seal(F))))),','(must(xtlm_import(C,F,inline)),log(imports(D))))))).
:-(xtl_token(A,B,C),','(assert(callable(A)),','('__contract_free_xtl_token'(A,B,C),assert(true)))).
:-('__contract_free_xtl_token'(A,B,C),','(dcg_call(A,B,D),xtl_skipwhite(D,C))).
test(:-(xtl_token,','(xtl_token('.'(120,[]),'.'(120,'.'(32,'.'(32,[]))),[]),','(xtl_token('.'(120,[]),'.'(120,'.'(32,'.'(37,'.'(32,'.'(99,'.'(111,'.'(109,'.'(109,'.'(101,'.'(110,'.'(116,[]))))))))))),[]),xtl_token('.'(120,[]),'.'(120,'.'(32,'.'(37,'.'(32,'.'(99,'.'(111,'.'(109,'.'(109,'.'(101,'.'(110,'.'(116,'.'(10,'.'(32,'.'(32,'.'(9,[]))))))))))))))),[]))))).
:-(xtl_skipwhite(A,B),','(xtl_white(A,C),','(!,=(C,B)))).
:-(xtl_skipwhite(A,B),=(A,B)).
test(:-(xtl_skipwhite,xtl_skipwhite([],[]))).
:-(xtl_white(A,B),','(append('.'(37,[]),C,A),','(','(!,=(C,D)),','(xtl_line_comment_(D,E),xtl_skipwhite(E,B))))).
:-(xtl_white(A,B),','(;(append('.'(32,[]),C,A),;(append('.'(9,[]),C,A),;(append('.'(13,[]),C,A),append('.'(10,[]),C,A)))),','(','(!,=(C,D)),xtl_skipwhite(D,B)))).
test(:-(xtl_white,','(xtl_white('.'(32,[]),[]),','(xtl_white('.'(37,[]),[]),xtl_white('.'(37,'.'(32,'.'(99,'.'(111,'.'(109,'.'(109,'.'(101,'.'(110,'.'(116,'.'(32,'.'(10,'.'(9,'.'(32,'.'(32,[])))))))))))))),[]))))).
:-(xtl_line_comment_(A,B),','(;(append('.'(10,[]),C,A),eof(A,C)),','(!,=(C,B)))).
:-(xtl_line_comment_(A,B),','(append('.'(C,[]),D,A),xtl_line_comment_(D,B))).
:-(xtl_top_level(A,B,C),','(assert(module(A)),','('__contract_free_xtl_top_level'(A,B,C),assert(module(A))))).
:-('__contract_free_xtl_top_level'(A,B,C),','(','(log(parsing_top_level),=(B,D)),','(;(','(append('.'(35,'.'(33,[])),E,D),','(','(!,=(E,F)),xtl_line_comment_(F,G))),','(true,=(D,G))),','(xtl_skipwhite(G,H),','(sf(many(xtl_declaration(A)),H,I),require(eof,I,C)))))).
:-(xtl_declaration(define(A,B,C)),','(atom(A),','(maplist(annotation,B),maplist(define_clause,C)))).
:-(xtl_declaration(test(A,B)),','(atom(A),xtl_goal(B))).
:-(xtl_declaration(prolog(/(A,B))),','(atom(A),number(B))).
:-(annotation(nondet),true).
:-(annotation(predicate),true).
:-(annotation(returns(A)),true).
:-(annotation(ensures(A)),xtl_goal(A)).
:-(annotation(requires(A)),xtl_goal(A)).
:-(annotation(traced),true).
:-(annotation(dcg),true).
:-(annotation(parameters(A)),maplist(var,A)).
:-(xtl_goal(A),true).
:-(define_clause(:(A,B)),true).
:-(xtl_declaration(A,B,C),','(assert(module(A)),','('__contract_free_xtl_declaration'(A,B,C),assert(module(A))))).
:-('__contract_free_xtl_declaration'(A,B,C),','(eof(B,D),','(','(!,=(D,E)),','(fail,=(E,C))))).
:-('__contract_free_xtl_declaration'(A,B,C),','(sf(require(xtl_expression(D)),B,E),','(require(xtl_token('.'(46,[])),E,F),','(','(!,=(F,G)),','(','(log(parsed_expression(D)),=(G,H)),','(;(','(=(D,include(I)),','(!,sf(xtl_include(I,A)))),;(','(=(D,import(I,J)),','(!,sf(xtl_import(I,J,A)))),','(!,','(sf(xtl_makevars(D,K,L)),','(!,','(sf(xtl_term_to_declaration(K,M)),','(!,sf(xtlm_add(A,M))))))))),=(H,C))))))).
:-(xtl_term_to_declaration(test(:(A,B)),test(A,B)),!).
:-(xtl_term_to_declaration(:(pred(A),','(contract(B,C,D),E)),define(A,'.'(nondet,'.'(predicate,'.'(parameters(F),'.'(requires(C),'.'(ensures(D),[]))))),G)),','(comma_list(E,G),comma_list(B,F))).
:-(xtl_term_to_declaration(:(pred(A),B),define(A,'.'(nondet,'.'(predicate,[])),C)),comma_list(B,C)).
:-(xtl_term_to_declaration(:(dcg(A),','(contract(B,C,D),E)),define(A,'.'(dcg,'.'(nondet,'.'(predicate,'.'(parameters(F),'.'(requires(C),'.'(ensures(D),[])))))),G)),','(comma_list(E,G),comma_list(B,F))).
:-(xtl_term_to_declaration(:(dcg(A),B),define(A,'.'(dcg,'.'(nondet,'.'(predicate,[]))),C)),comma_list(B,C)).
:-(xtl_term_to_declaration(:(fun(A),B),define(A,'.'(nondet,[]),C)),comma_list(B,C)).
:-(xtl_term_to_declaration(:(define(A),B),define(A,C,D)),xtl_split_annots(B,C,D)).
:-(xtl_term_to_declaration(prolog(A),prolog(A)),!).
:-(xtl_term_to_declaration(A,B),throw(failed(xtl_term_to_declaration(A)))).
:-(xtl_split_annots(A,B,C),','(comma_list(A,D),','(append(E,C,D),','(maplist(define_clause,C),maplist(xtl_make_annotation,E,B))))).
:-(xtl_make_annotation(A,parameters(B)),','(=...(A,'.'(parameters,B)),!)).
:-(xtl_make_annotation(C,C),true).
:-(xtl_makevars(A,B,C),','(=...(A,'.'('XTL$VARNAME','.'('_',[]))),!)).
:-(xtl_makevars(A,B,C),','(=...(A,'.'('XTL$VARNAME','.'(D,[]))),','(!,','(member(-(D,B),C),!)))).
:-(xtl_makevars(A,A,B),','(atomic(A),!)).
:-(xtl_makevars('.'(A,B),'.'(C,D),E),','(!,','(xtl_makevars(A,C,E),xtl_makevars(B,D,E)))).
:-(xtl_makevars(A,B,C),','(=...(A,D),','(!,','(xtl_makevars(D,E,C),=...(B,E))))).
test(:-(xtl_makevars,','(=...(A,'.'('XTL$VARNAME','.'('A',[]))),','(xtl_makevars(foo(A,A),foo(1,B),C),','(atomic(B),=(B,1)))))).
:-(xtl_atom_char(A,B,C),','(assert(true),','('__contract_free_xtl_atom_char'(A,B,C),assert(number(A))))).
:-('__contract_free_xtl_atom_char'(A,B,C),','(append('.'(A,[]),D,B),','(','(!,=(D,E)),','(member(A,'.'(97,'.'(98,'.'(99,'.'(100,'.'(101,'.'(102,'.'(103,'.'(104,'.'(105,'.'(106,'.'(107,'.'(108,'.'(109,'.'(110,'.'(111,'.'(112,'.'(113,'.'(114,'.'(115,'.'(116,'.'(117,'.'(118,'.'(119,'.'(120,'.'(121,'.'(122,'.'(65,'.'(66,'.'(67,'.'(68,'.'(69,'.'(70,'.'(71,'.'(72,'.'(73,'.'(74,'.'(75,'.'(76,'.'(77,'.'(78,'.'(79,'.'(80,'.'(81,'.'(82,'.'(83,'.'(84,'.'(85,'.'(86,'.'(87,'.'(88,'.'(89,'.'(90,'.'(95,'.'(48,'.'(49,'.'(50,'.'(51,'.'(52,'.'(53,'.'(54,'.'(55,'.'(56,'.'(57,'.'(36,'.'(63,[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),=(E,C))))).
:-(xtl_atom(A,B,C),','(append('.'(39,[]),D,B),','(xtl_quoted_atom_chars_(E,D,F),','(atom_codes(A,E),=(F,C))))).
:-(xtl_atom(A,B,C),','(many1(xtl_atom_char,D,B,E),','(','(!,=(E,F)),','(','(atom_codes(G,D),','(=(D,'.'(H,I)),;(','(=(H,95),','(!,=...(A,'.'('XTL$VARNAME','.'('_',[]))))),;(','(member(H,'.'(65,'.'(66,'.'(67,'.'(68,'.'(69,'.'(70,'.'(71,'.'(72,'.'(73,'.'(74,'.'(75,'.'(76,'.'(77,'.'(78,'.'(79,'.'(80,'.'(81,'.'(82,'.'(83,'.'(84,'.'(85,'.'(86,'.'(87,'.'(88,'.'(89,'.'(90,[]))))))))))))))))))))))))))),','(!,=...(A,'.'('XTL$VARNAME','.'(G,[]))))),=(A,G))))),=(F,C))))).
test(:-(xtl_atom,','(xtl_atom(a,'.'(39,'.'(97,'.'(39,[]))),[]),','(xtl_atom(+,'.'(39,'.'(43,'.'(39,[]))),[]),','(xtl_atom('9','.'(39,'.'(92,'.'(57,'.'(39,[])))),[]),','(xtl_atom(ab,'.'(97,'.'(98,[])),[]),xtl_atom('ab?','.'(97,'.'(98,'.'(63,[]))),[]))))))).
:-(xtl_quoted_char(A,B,C),','(append('.'(92,[]),D,B),','(','(!,=(D,E)),','(require('.'(F,[]),E,G),','(','(member(-(F,A),'.'(-(110,10),'.'(-(114,13),'.'(-(116,9),'.'(-(101,127),'.'(-(H,H),[])))))),=(G,I)),','(!,=(I,C))))))).
:-(xtl_quoted_atom_chars_([],A,B),','(append('.'(39,[]),C,A),','(!,=(C,B)))).
:-(xtl_quoted_atom_chars_('.'(A,B),C,D),','(xtl_quoted_char(A,C,E),','(','(!,=(E,F)),xtl_quoted_atom_chars_(B,F,D)))).
:-(xtl_quoted_atom_chars_('.'(A,B),C,D),','(append('.'(A,[]),E,C),xtl_quoted_atom_chars_(B,E,D))).
:-(xtl_expression(A,B,C),xtl_expression(1201,A,B,C)).
:-(xtl_expression(A,B,C,D),','(assert(number(A)),','('__contract_free_xtl_expression'(A,B,C,D),assert(true)))).
:-('__contract_free_xtl_expression'(A,B,C,D),xtl_expression(none,A,B,C,D)).
test(:-(xtl_expression,','(xtl_expression(1,'.'(49,[]),[]),','(xtl_expression(a,'.'(97,[]),[]),','(xtl_expression(+(a,b),'.'(97,'.'(32,'.'(43,'.'(32,'.'(98,[]))))),[]),','(xtl_expression(+(a,*(b,c)),'.'(97,'.'(32,'.'(43,'.'(32,'.'(98,'.'(32,'.'(42,'.'(32,'.'(99,[]))))))))),[]),','(xtl_expression(+(*(a,b),c),'.'(97,'.'(32,'.'(42,'.'(32,'.'(98,'.'(32,'.'(43,'.'(32,'.'(99,[]))))))))),[]),','(xtl_expression(*(-(a),b),'.'(45,'.'(97,'.'(32,'.'(42,'.'(32,'.'(98,[])))))),[]),','(xtl_expression(:-(*(a,b)),'.'(58,'.'(45,'.'(32,'.'(97,'.'(32,'.'(42,'.'(32,'.'(98,[])))))))),[]),','(xtl_expression('()','.'(40,'.'(32,'.'(41,[]))),[]),xtl_expression(:(x,:('()',y)),'.'(120,'.'(58,'.'(32,'.'(40,'.'(40,'.'(41,'.'(58,'.'(32,'.'(121,'.'(41,[])))))))))),[]))))))))))).
test(:-(comma_expr,xtl_expression(:-(p,','(a,b)),'.'(112,'.'(32,'.'(58,'.'(45,'.'(32,'.'(97,'.'(44,'.'(32,'.'(98,[]))))))))),[]))).
:-(xtl_regular_term(A,B,C),','(append('.'(48,'.'(39,[])),D,B),','(','(!,=(D,E)),','(require(xtl_string_char(A),E,F),xtl_skipwhite(F,C))))).
:-(xtl_regular_term(A,B,C),','(many1(digit,D,B,E),','(','(!,=(E,F)),','(','(','(foldl(add_digit,0,D,A),!),=(F,G)),xtl_skipwhite(G,C))))).
:-(xtl_regular_term(A,B,C),','(append('.'(34,[]),D,B),','(','(!,=(D,E)),','(require(many(xtl_string_char,A),E,F),','(require('.'(34,[]),F,G),xtl_skipwhite(G,C)))))).
:-(xtl_regular_term(A,B,C),','(xtl_atom(D,B,E),','(','(!,=(E,F)),;(','(xtl_token('.'(40,[]),F,G),','(','(!,=(G,H)),','(xtl_comma_separated(I,[],xtl_token('.'(41,[])),H,J),','(=...(A,'.'(D,I)),=(J,C))))),','(xtl_skipwhite(F,K),','(=(A,D),=(K,C))))))).
:-(xtl_regular_term('()',A,B),','(xtl_token('.'(40,[]),A,C),','(xtl_token('.'(41,[]),C,D),','(!,=(D,B))))).
:-(xtl_regular_term(A,B,C),','(xtl_token('.'(40,[]),B,D),','(xtl_expression(A,D,E),require(xtl_token('.'(41,[])),E,C)))).
:-(xtl_regular_term({}(A),B,C),','(xtl_token('.'(123,[]),B,D),','(xtl_expression(A,D,E),require(xtl_token('.'(125,[])),E,C)))).
:-(xtl_regular_term(A,B,C),','(xtl_token('.'(91,[]),B,D),','(xtl_comma_separated(A,E,;(','(xtl_token('.'(93,[])),{}(=(E,[]))),','(xtl_token('.'(124,[])),','(xtl_expression(E),xtl_token('.'(93,[]))))),D,F),','(!,=(F,C))))).
:-(xtl_string_char(A,B,C),','(assert(true),','('__contract_free_xtl_string_char'(A,B,C),assert(byte(A))))).
:-('__contract_free_xtl_string_char'(A,B,C),','(append('.'(34,[]),D,B),','(','(!,=(D,E)),','(false,=(E,C))))).
:-('__contract_free_xtl_string_char'(A,B,C),','(xtl_quoted_char(A,B,D),','(!,=(D,C)))).
:-('__contract_free_xtl_string_char'(A,B,C),append('.'(A,[]),C,B)).
test(:-(xtl_regular_term,','(xtl_regular_term(123,'.'(49,'.'(50,'.'(51,[]))),[]),','(xtl_regular_term(hi,'.'(104,'.'(105,[])),[]),','(xtl_regular_term(hi(1),'.'(104,'.'(105,'.'(40,'.'(49,'.'(41,[]))))),[]),','(xtl_regular_term(hi(b,4),'.'(104,'.'(105,'.'(40,'.'(98,'.'(44,'.'(32,'.'(52,'.'(41,[])))))))),[]),','(xtl_regular_term(6,'.'(40,'.'(54,'.'(41,[]))),[]),','(xtl_regular_term({}(x),'.'(123,'.'(120,'.'(125,[]))),[]),','(xtl_regular_term([],'.'(91,'.'(93,[])),[]),xtl_regular_term('.'(1,'.'(2,'.'(3,[]))),'.'(91,'.'(49,'.'(44,'.'(50,'.'(44,'.'(51,'.'(93,[]))))))),[])))))))))).
:-(xtl_comma_separated(A,B,C,D,E),xtl_comma_seperated_first(A,B,C,D,E)).
:-(xtl_comma_seperated_first(A,A,B,C,D),','(dcg_call(B,C,E),','(!,=(E,D)))).
:-(xtl_comma_seperated_first('.'(A,B),C,D,E,F),','(xtl_expression(1000,A,E,G),','(','(!,=(G,H)),xtl_comma_separated_next(B,C,D,H,F)))).
:-(xtl_comma_separated_next(A,A,B,C,D),','(dcg_call(B,C,E),','(!,=(E,D)))).
:-(xtl_comma_separated_next('.'(A,B),C,D,E,F),','(require(xtl_token('.'(44,[])),E,G),','(','(!,=(G,H)),','(xtl_expression(1000,A,H,I),','(','(!,=(I,J)),xtl_comma_separated_next(B,C,D,J,F)))))).
:-(xtl_op_or_term(!,term,A,B),','(append('.'(33,[]),C,A),xtl_skipwhite(C,B))).
:-(xtl_op_or_term(A,B,C,D),','(xtl_regular_term(A,C,E),','(','(!,=(E,F)),;(','(','(xtl_op(G,H,A),=(B,op(G,H))),=(F,D)),','(=(B,term),=(F,D)))))).
:-(xtl_op_or_term(A,B,C,D),','(many1(xtl_op_char,E,C,F),','(','(xtl_known_op(E,A,G,H,F,I),','(=(B,op(G,H)),=(I,J))),xtl_skipwhite(J,D)))).
:-(xtl_known_op(A,B,C,D,E,F),','(','(atom_codes(B,A),','(xtl_op(G,H,B),','(!,xtl_op(C,D,B)))),=(E,F))).
:-(xtl_known_op(A,B,C,D,E,F),','(','(append(G,'.'(H,[]),A),=(E,I)),','(append('.'(H,[]),I,J),xtl_known_op(G,B,C,D,J,F)))).
:-(xtl_op_char(A,B,C),','(append('.'(A,[]),D,B),','(','(member(A,'.'(96,'.'(126,'.'(33,'.'(64,'.'(35,'.'(37,'.'(94,'.'(38,'.'(42,'.'(60,'.'(62,'.'(47,'.'(59,'.'(58,'.'(45,'.'(95,'.'(61,'.'(43,'.'(44,'.'(124,'.'(92,'.'(46,[]))))))))))))))))))))))),=(D,E)),','(!,=(E,C))))).
:-(xtl_expression(none,A,B,C,D),','(xtl_op_or_term(E,op(F,G),C,H),','(','(','(member(-(G,I),'.'(-(fx,0),'.'(-(fy,1),[]))),is(J,+(F,I))),=(H,K)),','(try(xtl_expression(none,J,L),K,M),','(','(=...(N,'.'(E,'.'(L,[]))),=(M,O)),xtl_expression(just(N),A,B,O,D)))))).
:-(xtl_expression(none,A,B,C,D),','(','(!,=(C,E)),','(require(xtl_op_or_term(F,term),E,G),xtl_expression(just(F),A,B,G,D)))).
:-(xtl_expression(just(A),B,C,D,E),','(xtl_op_or_term(F,op(G,H),D,I),','(','(','(member(-(H,J),'.'(-(xf,0),'.'(-(yf,1),[]))),','(is(K,+(G,J)),','(<(K,B),','(!,=...(L,'.'(F,'.'(A,[]))))))),=(I,M)),xtl_expression(just(L),B,C,M,E)))).
:-(xtl_expression(just(A),B,C,D,E),','(xtl_op_or_term(F,op(G,H),D,I),','(','(','(member(-(-(H,J),K),'.'(-(-(xfx,0),0),'.'(-(-(xfy,0),1),'.'(-(-(yfx,1),0),[])))),','(is(L,+(G,J)),','(<(L,B),','(!,is(M,+(G,K)))))),=(I,N)),','(require(xtl_expression(none,M,O),N,P),','(','(=...(Q,'.'(F,'.'(A,'.'(O,[])))),=(P,R)),xtl_expression(just(Q),B,C,R,E)))))).
:-(xtl_expression(just(A),B,A,C,D),','(!,=(C,D))).
:-(xtl_op(1200,xfx,:-),true).
:-(xtl_op(1200,xfx,-->),true).
:-(xtl_op(1200,fx,:-),true).
:-(xtl_op(1105,xfy,'|'),true).
:-(xtl_op(1100,xfy,;),true).
:-(xtl_op(1050,xfy,->),true).
:-(xtl_op(1000,xfy,','),true).
:-(xtl_op(900,fy,not),true).
:-(xtl_op(700,xfx,=),true).
:-(xtl_op(700,xfx,\=),true).
:-(xtl_op(700,xfx,=..),true).
:-(xtl_op(700,xfx,==),true).
:-(xtl_op(700,xfx,\==),true).
:-(xtl_op(700,xfx,is),true).
:-(xtl_op(700,xfx,<),true).
:-(xtl_op(700,xfx,>),true).
:-(xtl_op(700,xfx,=<),true).
:-(xtl_op(700,xfx,>=),true).
:-(xtl_op(700,xfx,=\=),true).
:-(xtl_op(500,yfx,+),true).
:-(xtl_op(500,yfx,-),true).
:-(xtl_op(400,yfx,*),true).
:-(xtl_op(400,yfx,/),true).
:-(xtl_op(400,yfx,rem),true).
:-(xtl_op(400,yfx,mod),true).
:-(xtl_op(400,yfx,div),true).
:-(xtl_op(400,yfx,<<),true).
:-(xtl_op(400,yfx,>>),true).
:-(xtl_op(200,xfx,**),true).
:-(xtl_op(200,xfx,^),true).
:-(xtl_op(200,fy,+),true).
:-(xtl_op(200,fy,-),true).
:-(xtl_op(1200,fy,test),true).
:-(xtl_op(999,fx,tc),true).
:-(xtl_op(999,fx,sf),true).
:-(xtl_op(700,xfx,=...),true).
:-(xtl_op(1200,xfy,:),true).
:-(xtl_op(1150,fy,dcg),true).
:-(xtl_op(1150,fy,pred),true).
:-(xtl_op(1150,fy,include),true).
:-(xtl_op(1150,fy,fun),true).
:-(xtl_op(1150,fy,define),true).
:-(xtl_op(700,xfx,:=),true).
:-(xtl_op(1125,xfx,if),true).
:-(xtl_op(100,fy,'`'),true).
:-(xtl_op(700,fx,:=),true).
:-(xtl_slim_declaration(test(:(A,B)),[]),true).
:-(xtl_slim_declaration(C,C),true).
:-(xtl_to_pl_toplevel(A,B),','(maplist(must(xtl_to_pl_declaration),A,C),','(append(C,D),append('.'(:-(set_prolog_flag(singleton_warning,off)),'.'(:-(discontiguous(/(test,1))),'.'(:-(initialization(;(main,','(write('error: unexpected failure in main'),','(nl,halt(1)))))),[]))),D,B)))).
:-(xtl_to_pl_declaration(-(A,B),C),','(log(compiling(B,A)),sf(xtl_to_pl_declaration(A,B,C)))).
:-(xtl_to_pl_declaration(A,B,C),','(assert(xtl_declaration(B)),','('__contract_free_xtl_to_pl_declaration'(A,B,C),assert(true)))).
:-('__contract_free_xtl_to_pl_declaration'(A,B,C),','(log(to_pl_declaration(B)),false)).
:-('__contract_free_xtl_to_pl_declaration'(A,test(B,C),'.'(test(:-(B,D)),[])),','(!,','(xtl_to_pl_goal(A,C,D),numbervars(D)))).
:-('__contract_free_xtl_to_pl_declaration'(A,define(B,'.'(nondet,[]),C),D),','(!,','(maplist(xtl_to_pl_fun(A,B),C,D),numbervars(D)))).
:-('__contract_free_xtl_to_pl_declaration'(A,define(B,'.'(nondet,'.'(predicate,C)),D),E),','(!,;(','(=(C,[]),maplist(xtl_def_to_pl(A,pred(B)),D,E)),','(atom_concat('__contract_free_',B,F),','(maplist(xtl_def_to_pl(A,pred(F)),D,G),','(xtl_contract_to_pl(A,pred,B,F,C,H),append(H,G,E))))))).
:-('__contract_free_xtl_to_pl_declaration'(A,define(B,'.'(dcg,'.'(nondet,'.'(predicate,C))),D),E),','(!,;(','(=(C,[]),maplist(xtl_def_to_pl(A,dcg(B)),D,E)),','(atom_concat('__contract_free_',B,F),','(maplist(xtl_def_to_pl(A,dcg(F)),D,G),','(xtl_contract_to_pl(A,dcg,B,F,C,H),append(H,G,E))))))).
:-('__contract_free_xtl_to_pl_declaration'(A,define(B,C,D),E),','(maplist(xtl_annot_prepare,C,F),','(xtl_define_clauses_to_params(D,G),','(foldl(xtl_annot_extend_params,G,F,H),','(xtl_annot_wrap(A,B,H,F,','(I,J)),','(maplist(xtl_annot_clause_to_pl(A,J,F),D,K),append(I,K,E))))))).
:-('__contract_free_xtl_to_pl_declaration'(A,prolog(/(B,C)),[]),!).
:-('__contract_free_xtl_to_pl_declaration'(A,B,C),','(!,throw(error(unknown_declaration(B))))).
test(:-(xtl_to_pl_declaration,','(xtlm_new(main,A),','(xtlm_add(A,prolog(/(e,0))),','(xtl_to_pl_declaration(A,define(odd,'.'(nondet,'.'(predicate,[])),'.'(:(0,false),'.'(:(1,true),'.'(:(B,','(is(C,-(B,2)),odd(C))),[])))),D),','(=(D,'.'(:-(odd(0),false),'.'(:-(odd(1),true),'.'(:-(odd(E),','(is(F,-(E,2)),odd(F))),[])))),','(xtl_to_pl_declaration(A,define(f,'.'(dcg,'.'(nondet,'.'(predicate,[]))),'.'(:(x,'.'(x,[])),[])),G),','(=(G,'.'(:-(f(x,H,I),append('.'(x,[]),I,H)),[])),','(xtl_to_pl_declaration(A,define(f,'.'(nondet,'.'(predicate,'.'(parameters('.'(J,[])),'.'(requires(b),'.'(ensures(c),[]))))),'.'(:(d,e),[])),K),','(=(K,'.'(:-(f(L),','(assert(b),','('__contract_free_f'(L),assert(c)))),'.'(:-('__contract_free_f'(d),e),[]))),','(xtl_to_pl_declaration(A,define(f,'.'(dcg,'.'(nondet,'.'(predicate,'.'(parameters('.'(M,[])),'.'(requires(b),'.'(ensures(c),[])))))),'.'(:(d,e),[])),N),','(=(N,'.'(:-(f(O,J,P),','(assert(b),','('__contract_free_f'(O,J,P),assert(c)))),'.'(:-('__contract_free_f'(d,Q,R),e(Q,R)),[]))),','(xtl_to_pl_declaration(A,define(f,'.'(nondet,'.'(predicate,[])),'.'(:('()',','(g,h)),[])),S),','(=(S,'.'(:-(f,','(g,h)),[])),','(xtl_to_pl_declaration(A,define(fib,'.'(nondet,[]),'.'(:(0,1),'.'(:(1,1),'.'(:(B,+(fib(-(B,1)),fib(-(B,2)))),[])))),T),=(T,'.'(:-(fib(0,1),true),'.'(:-(fib(1,1),true),'.'(:-(fib(U,V),','(-(U,1,W),','(fib(W,X),','(-(U,2,Y),','(fib(Y,Z),+(X,Z,V)))))),[]))))))))))))))))))).
:-(xtl_def_to_pl(A,dcg(B),:(C,D),:-(E,F)),','(copy_term(-(C,D),-(G,H)),','(comma_list(G,I),','(append(I,'.'(J,'.'(K,[])),L),','(=...(E,'.'(B,L)),','(xtl_to_pl_dcg(A,H,F,J,K),numbervars(-(E,F)))))))).
:-(xtl_def_to_pl(A,pred(B),:(C,D),:-(E,F)),','(copy_term(-(C,D),-(G,H)),','(comma_list(G,I),','(=...(E,'.'(B,I)),','(xtl_to_pl_goal(A,H,F),numbervars(-(E,F))))))).
:-(xtl_to_pl_goal(A,B,call(B)),','(var(B),!)).
:-(xtl_to_pl_goal(A,','(B,C),','(D,E)),','(!,','(xtl_to_pl_goal(A,B,D),xtl_to_pl_goal(A,C,E)))).
:-(xtl_to_pl_goal(A,;(B,C),;(D,E)),','(!,','(xtl_to_pl_goal(A,B,D),xtl_to_pl_goal(A,C,E)))).
:-(xtl_to_pl_goal(A,;(->(B,C),D),;(->(E,F),G)),','(!,','(xtl_to_pl_goal(A,B,E),','(xtl_to_pl_goal(A,C,F),xtl_to_pl_goal(A,D,G))))).
:-(xtl_to_pl_goal(A,->(B,C),->(D,E)),','(!,','(xtl_to_pl_goal(A,B,D),xtl_to_pl_goal(A,C,E)))).
:-(xtl_to_pl_goal(A,!,!),!).
:-(xtl_to_pl_goal(A,:=(B,C),D),','(!,','(xtl_to_pl_funexpr(A,B,C,E),comma_list(D,E)))).
:-(xtl_to_pl_goal(A,B,C),','(=...(B,'.'(D,E)),','(maplist(xtl_to_pl_quoted(A),F,E,G),','(append(G,H),','(comma_list(I,H),','(=...(J,'.'(D,F)),;(','(=(I,'()'),=(C,J)),=(C,','(I,J))))))))).
:-(xtl_to_pl_dcg(A,B,dcg_call(B,C,D),C,D),','(var(B),!)).
:-(xtl_to_pl_dcg(A,','(B,C),','(D,E),F,G),','(!,','(xtl_to_pl_dcg(A,B,D,F,H),xtl_to_pl_dcg(A,C,E,H,G)))).
:-(xtl_to_pl_dcg(A,;(B,C),;(D,E),F,G),','(!,','(xtl_to_pl_dcg(A,B,D,F,G),xtl_to_pl_dcg(A,C,E,F,G)))).
:-(xtl_to_pl_dcg(A,!,','(!,=(B,C)),B,C),!).
:-(xtl_to_pl_dcg(A,[],=(B,C),B,C),!).
:-(xtl_to_pl_dcg(A,'.'(B,C),append('.'(B,C),D,E),E,D),!).
:-(xtl_to_pl_dcg(A,{}(B),','(C,=(D,E)),D,E),','(!,xtl_to_pl_goal(A,B,C))).
:-(xtl_to_pl_dcg(A,B,C,D,E),','(!,','(=...(B,'.'(F,G)),','(','(length(G,H),+(H,2,I)),;(','(maplist(xtl_to_pl_quoted(A),J,G,K),','(append('.'(F,J),'.'(D,'.'(E,[])),L),','(=...(M,L),','(append(K,N),;(','(=(N,[]),','(!,=(C,M))),','(comma_list(O,N),=(C,','(O,M)))))))),;(','(','(-(I,2,P),xtlm_find(A,/(/(F,P),Q))),','(xtl_to_pl_goal(A,B,R),=(C,','(=(D,E),R)))),throw(not_found(/(F,I))))))))).
:-(xtl_to_pl_dcg(A,B,C,D,E),throw(error(xtl_to_pl_dcg,B))).
test(:-(xtl_to_pl_dcg,','(xtlm_new(main,A),','(xtlm_add(A,prolog(/(f,0))),','(xtlm_add(A,prolog(/(g,0))),','(xtlm_add(A,prolog(/(h,0))),','(xtlm_add(A,prolog(/(e,0))),','(xtlm_add(A,prolog(/(i,0))),','(xtl_to_pl_dcg(A,','(;(f,g),h),','(;(f(a,b),g(a,b)),h(b,c)),a,c),','(xtl_to_pl_dcg(A,','(e,','(;(','(f,i),g),h)),','(e(a,b),','(;(','(f(b,c),i(c,d)),g(b,d)),h(d,e))),a,e),','(xtl_to_pl_dcg(A,B,C,i,o),','(==(C,dcg_call(B,i,o)),','(xtl_to_pl_dcg(A,{}(B),D,i,o),==(D,','(call(B),=(i,o)))))))))))))))).
:-(xtl_contract_to_pl(A,pred,B,C,D,'.'(E,[])),','(member(parameters(F),D),','(!,','(;(member(requires(G),D),=(G,true)),','(!,','(;(member(ensures(H),D),=(H,true)),','(!,','(=...(I,'.'(B,F)),','(xtl_to_pl_goal(A,G,J),','(xtl_to_pl_goal(A,H,K),','(=...(L,'.'(C,F)),','(=(M,:-(I,','(assert(G),','(L,assert(H))))),','(copy_term(M,E),numbervars(E)))))))))))))).
:-(xtl_contract_to_pl(A,dcg,B,C,'.'(parameters(D),'.'(requires(E),'.'(ensures(F),[]))),G),','(append(D,'.'(H,'.'(I,[])),J),xtl_contract_to_pl(A,pred,B,C,'.'(parameters(J),'.'(requires(E),'.'(ensures(F),[]))),G))).
:-(xtl_to_pl_fun(A,B,:(C,D),:-(E,F)),','(comma_list(C,G),','(=...(H,'.'(B,G)),','(append(G,'.'(I,[]),J),','(=...(E,'.'(B,J)),','(xtl_to_pl_funexpr(A,I,D,K),;(','(=(K,[]),=(F,true)),comma_list(F,K)))))))).
:-(xtl_to_pl_funexpr(A,B,B,[]),','(var(B),!)).
:-(xtl_to_pl_funexpr(A,B,if(B,C),'.'(D,[])),','(!,xtl_to_pl_goal(A,C,D))).
:-(xtl_to_pl_funexpr(A,B,B,[]),','(number(B),!)).
:-(xtl_to_pl_funexpr(A,B,B,[]),','(atom(B),!)).
:-(xtl_to_pl_funexpr(A,[],[],[]),true).
:-(xtl_to_pl_funexpr(A,'.'(B,C),'.'(D,E),F),','(xtl_to_pl_funexpr(A,B,D,G),','(xtl_to_pl_funexpr(A,C,E,H),append(G,H,F)))).
:-(xtl_to_pl_funexpr(A,B,'`'(C),D),xtl_to_pl_quoted(A,B,C,D)).
:-(xtl_to_pl_funexpr(A,B,C,D),','(compound(C),','(!,','(=...(C,'.'(E,F)),','(maplist(xtl_to_pl_funexpr(A),G,F,H),','(append(H,I),','(append(G,'.'(B,[]),J),','(=...(K,'.'(E,J)),append(I,'.'(K,[]),D))))))))).
test(:-(xtl_to_pl_funexpr,','(xtlm_new(main,A),','(xtl_to_pl_funexpr(A,res,f(1,g(2,3)),B),','(=(B,'.'(g(2,3,C),'.'(f(1,C,res),[]))),','(xtl_to_pl_funexpr(A,res,+(fib(-(D,1)),fib(-(D,2))),E),=(E,'.'(-(C,1,F),'.'(fib(F,G),'.'(-(C,2,H),'.'(fib(H,I),'.'(+(G,I,res),[])))))))))))).
:-(xtl_to_pl_quoted(A,B,C,[]),','(var(C),','(!,=(B,C)))).
:-(xtl_to_pl_quoted(A,B,:=(C),D),','(!,xtl_to_pl_funexpr(A,B,C,D))).
:-(xtl_to_pl_quoted(A,B,B,[]),','(number(B),!)).
:-(xtl_to_pl_quoted(A,B,B,[]),','(atom(B),!)).
:-(xtl_to_pl_quoted(A,B,C,D),','(compound(C),','(!,','(=...(C,'.'(E,F)),','(maplist(xtl_to_pl_quoted(A),G,F,H),','(append(H,D),=...(B,'.'(E,G)))))))).
:-(:=(A,B),call(B,A)).
:-(xtl_define_clauses_to_params('.'(:(A,B),C),append(D,'.'(E,[]))),','(comma_list(A,F),','(length(F,G),length(D,G)))).
:-(xtl_annot_prepare(A,A),true).
:-(xtl_annot_extend_params(A,predicate,B),append(B,'.'(C,[]),A)).
:-(xtl_annot_extend_params(A,dcg,D),append(A,'.'(E,'.'(F,[])),D)).
:-(xtl_annot_extend_params(A,G,A),true).
:-(xtl_annot_wrap_maybe(A,B,C,D,E),;(','(xtl_annot_should_wrap(D),xtl_annot_wrap(A,B,C,D,E)),=(E,','([],B)))).
:-(xtl_annot_should_wrap('.'(requires(A),B)),!).
:-(xtl_annot_should_wrap('.'(ensures(A),B)),!).
:-(xtl_annot_should_wrap('.'(A,B)),xtl_annot_should_wrap(B)).
:-(xtl_annot_wrap(A,B,C,D,'`'(','(E,:=(F)))),','(atom_concat('__wrap_',B,F),','(=(G,parameters(C)),','(;(','(member(G,D),=(H,D)),=(H,'.'(G,D))),xtl_contract_to_pl(A,pred,B,F,H,E))))).
:-(xtl_annot_clause_to_pl(A,B,C,D,E),;(','(member(dcg,C),','(!,maplist(xtl_def_to_pl(A,dcg(B)),D,E))),;(','(member(predicate,C),','(!,maplist(xtl_def_to_pl(A,pred(B)),D,E))),maplist(xtl_to_pl_fun(A,B),D,E)))).
:-(pl_write_top_level([],A,B),=(A,B)).
:-(pl_write_top_level('.'(A,B),C,D),','(pl_write_term(A,C,E),','(append('.'(46,'.'(10,[])),F,E),pl_write_top_level(B,F,D)))).
:-(pl_write_term(A,B,C),','(','(','(open_output_codes_stream(D),','(write_term(D,A,'.'(quoted(true),'.'(namevars(true),'.'(numbervars(true),'.'(ignore_ops(true),[]))))),close_output_codes_stream(D,E))),=(B,F)),dcg_call(E,F,C))).
:-(module(module(A,B,C)),name(A)).
:-(module_item(-(A,B)),','(module_key(A),module_declaration(B))).
:-(module_key(/(/(A,B),C)),','(name(A),','(;(number(B),member(B,'.'(-,[]))),member(C,'.'(test,'.'(imported,'.'(pred,'.'(dcg,'.'(fun,[]))))))))).
:-(module_declaration(imported(A,B)),','(!,','(name(A),name(B)))).
:-(module_declaration(prolog(/(A,B))),','(!,','(atom(A),number(B)))).
:-(module_declaration(A),xtl_declaration(A)).
:-(name(name(A)),','(maplist(atom,A),\+(length(A,1)))).
:-(name(A),atom(A)).
:-(name_list(name(A),A),\+(=(A,'.'(B,[])))).
:-(name_list(A,'.'(A,[])),atom(A)).
:-(concat_name(A,B,C),','(','(name_list(A,D),','(name_list(B,E),append(D,E,F))),name_list(C,F))).
:-(xtlm_new(A,B),','(log(new_module(A)),=(B,module(A,C,'.'(-(A,C),D))))).
:-(xtlm_new(A,B,C),','(','(plength(B,D),log(new(A,D))),','(;(','(pmember(-(A,E),B),throw(error(already_imported(A)))),true),','(pinsert(-(A,F),B),=(C,module(A,F,B)))))).
:-(xtlm_find(A,/(/(B,C),D)),xtlm_find_(A,-(/(/(B,C),D),E))).
:-(xtlm_find_(module(A,B,C),D),pmember(D,B)).
:-(xtlm_add(A,B),','(xtlm_declaration_key(B,/(/(C,D),E)),;(','(xtlm_find(A,/(/(C,D),F)),throw(conflicting_declaration(/(C,D),F,B))),xtlm_add_(A,/(/(C,D),E),B)))).
:-(xtlm_add_(module(A,B,C),D,E),;(pinsert(-(D,E),B),throw(error('xtlm_add_: module is already sealed')))).
:-(xtlm_declaration_key(prolog(/(A,B)),/(/(A,B),pred)),true).
:-(xtlm_declaration_key(define(A,B,C),D),','(!,','(=(C,'.'(:(E,F),G)),;(','(member(fun,B),','(!,','(','(comma_list(E,H),','(length(H,I),+(1,I,J))),=(D,/(/(A,J),fun))))),;(','(member(dcg,B),','(!,','(','(comma_list(E,K),','(length(K,L),+(2,L,M))),=(D,/(/(A,M),dcg))))),','(!,','(','(comma_list(E,N),length(N,O)),=(D,/(/(A,O),pred))))))))).
:-(xtlm_declaration_key(test(A,B),/(/(A,test),test)),!).
:-(xtlm_declaration_key(A,B),throw(error(unsuported_declaration(A)))).
test(:-(xtlm_declaration_key,xtlm_declaration_key(define(foo,'.'(fun,[]),'.'(:(A,+(A,1)),[])),/(/(foo,2),fun)))).
:-(xtlm_seal(module(A,B,C)),must(length(B,D))).
:-(xtlm_import(A,B,C),','(must(=(B,module(D,E,F))),','(must(=(A,module(G,H,F))),','(must(;(','(=(C,qualified),','(!,=(I,D))),;(','(=(C,as(I)),!),','(=(C,inline),=(I,name([])))))),maplist(must(xtlm_add_import_(A,I,D)),E))))).
:-(xtlm_add_import_(A,B,C,-(D,E)),','(must(=(/(/(F,G),H),D)),','(must(:=(I,concat_name(B,F))),must(xtlm_add_(A,/(/(I,G),H),imported(C,F)))))).
:-(xtlm_zip_module_(A,B,C),=(C,-(A,B))).
:-(xtlm_all_declarations_(A,-(B,C),D),','(maplist(snd,C,E),','(filterlist(xtl_declaration,E,F),','(=(G,module(B,C,A)),maplist(xtlm_zip_module_(G),F,D))))).
:-(xtlm_all_declarations_recursive(module(A,B,C),D),','(length(C,E),','(maplist(xtlm_all_declarations_(C),C,F),append(F,D)))).
:-(xtlm_imports(module(A,B,C),C),true).
:-(main,catch(','(current_prolog_flag(argv,A),;(','(=(A,'.'(B,'.'(C,D))),','(!,','(command(C,D),halt))),','(command(help,[]),halt(1)))),E,','(write('failed: '),','(print_exception(E),halt(1))))).
:-(command(help,A),','(write('extol repl'),','(nl,','(write('    Invoke the interactive REPL'),','(nl,','(write('extol extoltoprolog <in.xtl> <out.pl> [--slim]'),','(nl,','(write('    Convert the input for to prolog. Use "--sim" to remove tests for bootstrapping'),','(nl,','(write('extol test [test-name]'),','(nl,','(write('    Run all tests or, if present, just the named test'),nl)))))))))))).
:-(command(test,A),;(','(test(:-(B,C)),','(;(=('.'(B,[]),A),=(A,[])),','(write('[ ] + '),','(write(B),','(write(': '),','(once(run_test(C)),fail)))))),true)).
:-(command(extoltoprolog,'.'(A,'.'(B,C))),','(xtlm_new(main,D),','(log(revving),','(must(xtl_include(A,D)),','(must(xtlm_seal(D)),','(log(parsed),','(must(xtlm_all_declarations_recursive(D,E)),','(','(length(E,F),log(listed(F))),','(;(','(member('--slim',C),','(=(G,true),!)),=(G,false)),','(log(decls_new(E)),','(must(xtl_to_pl_toplevel(E,H)),','(','(length(H,I),log(translated(I))),','(must(pl_write_top_level(H,J,[])),','(!,','(must(append('.'(37,'.'(32,'.'(71,'.'(101,'.'(110,'.'(101,'.'(114,'.'(97,'.'(116,'.'(101,'.'(100,'.'(32,'.'(98,'.'(121,'.'(32,'.'(101,'.'(120,'.'(116,'.'(111,'.'(108,'.'(116,'.'(111,'.'(112,'.'(114,'.'(111,'.'(108,'.'(111,'.'(103,'.'(10,[]))))))))))))))))))))))))))))),J,K)),must(write_file(B,K))))))))))))))))).
:-(command(repl,[]),xtl_repl).
:-(command(A,B),','(write(unknown_command(A)),','(nl,false))).
:-(run_test(done),','(!,','(write(success),nl))).
:-(run_test(','(A,B)),','(!,;(->(sf(A),','(!,run_test(B))),','(nl,','(write('  failed: '),','(write_term(A,'.'(numbervars(false),'.'(namevars(false),'.'(max_depth(6),[])))),','(nl,throw(test_failed)))))))).
:-(run_test(A),run_test(','(A,done))).
:-(xtl_repl,','(xtlm_new(repl,A),','(write('Extol> '),','(flush_output,','(read_line(B),','(;(=(B,'.'(10,[])),;(','(=(B,[]),','(write(end),halt)),;(catch(','(;(xtl_expression(C,B,[]),throw('parse failed')),','(xtl_makevars(C,D,E),','(xtl_to_pl_funexpr(A,F,D,G),','(comma_list(H,G),','(;(=(H,'()'),call(H)),','(write(F),nl)))))),I,','(write('failed: '),print_exception(I))),','(write(error),nl)))),','(!,xtl_repl))))))).
:-(xtl_repl,','(write(end),nl)).
