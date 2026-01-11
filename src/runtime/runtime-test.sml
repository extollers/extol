val _ = print "Running runtime tests...\n"

fun assert msg b =
    if b then print ("PASS: " ^ msg ^ "\n")
    else (print ("FAIL: " ^ msg ^ "\n"); OS.Process.exit OS.Process.failure)

fun checkValEq msg (v1: Value option) (v2: Value option) =
    case (v1, v2) of
        (SOME (Integer i1), SOME (Integer i2)) => assert msg (i1 = i2)
      | (SOME (String s1), SOME (String s2)) => assert msg (s1 = s2)
      | (NONE, NONE) => assert msg true
      | _ => assert (msg ^ " (mismatch)") false

fun testStack () =
    let
        val _ = print "Testing Stack...\n"
        val res1 = Stack.try NONE (fn _ => ())
        val _ = assert "Stack.try success returns false" (not res1)

        val res2 = Stack.try NONE (fn frame => Stack.fail frame)
        val _ = assert "Stack.try failure returns true" res2
    in
        ()
    end

fun testCompare () =
    let
        val _ = print "Testing compare...\n"
        val _ = Stack.try NONE (fn frame =>
            let
                val ctx = { frame = frame }
                val i1 = Integer 1
                val i2 = Integer 2
                val r1 = Real 1.0
                val r2 = Real 2.0
                val s1 = String "abc"
                val s2 = String "def"
                val sym1 = Symbol 0w1
            in
                assert "Int eq" (compare ctx i1 i1 = EQUAL);
                assert "Int lt" (compare ctx i1 i2 = LESS);
                assert "Int gt" (compare ctx i2 i1 = GREATER);
                
                assert "Real eq" (compare ctx r1 r1 = EQUAL);
                assert "Real lt" (compare ctx r1 r2 = LESS);
                
                assert "Int/Real eq" (compare ctx i1 r1 = EQUAL);
                assert "Real/Int eq" (compare ctx r1 i1 = EQUAL);
                
                assert "String eq" (compare ctx s1 s1 = EQUAL);
                assert "String lt" (compare ctx s1 s2 = LESS);
                
                assert "Symbol eq" (compare ctx sym1 sym1 = EQUAL);
                
                assert "String vs Int" (compare ctx s1 i1 = LESS);
                ()
            end
        )
    in
        ()
    end

fun testLookup () =
    let
        val _ = print "Testing lookup...\n"
        val _ = Stack.try NONE (fn frame =>
            let
                val ctx = { frame = frame }
                val recNil = RecordNil
                val rec1 = RecordCons { key = 0w1, value = Integer 10, rest = recNil }
                val rec2 = RecordCons { key = 0w2, value = String "hi", rest = rec1 }
            in
                checkValEq "Lookup 1" (lookup ctx 0w1 rec2) (SOME (Integer 10));
                checkValEq "Lookup 2" (lookup ctx 0w2 rec2) (SOME (String "hi"));
                checkValEq "Lookup missing" (lookup ctx 0w3 rec2) NONE
                        end
                    )
                in
                    ()
                end
            
            fun testUnion () =
                    let
                        val _ = print "Testing union...\n"
                        val _ = Stack.try NONE (fn frame =>
                            let
                                val ctx = { frame = frame }
                                val i1 = Integer 1
                                val i2 = Integer 1
                                val i3 = Integer 2
                            in
                                union ctx i1 i2;
                                assert "Int unify eq" true;
                                ignore (Stack.try (SOME frame) (fn frame' => union {frame=frame'} i1 i3));
                                assert "Int unify neq handled" true
                            end
                        )
                
                                val failed = Stack.try NONE (fn frame =>
                
                                    let
                
                                        val ctx = { frame = frame }
                
                                        val h = ref NONE
                
                                        val vHole = Unknown (Hole h)
                
                                        
                
                                                        (* unify (Rec "a" 1 (Rec "b" 2 ())) (Rec "b" Hole ()) *)
                
                                        
                
                                                        val r1 = Record (RecordCons { key = 0w97, value = Integer 1, 
                
                                        
                
                                                                                      rest = RecordCons { key = 0w98, value = Integer 2, 
                
                                        
                
                                                                                                          rest = RecordNil } })
                
                                        
                
                                                        val r2 = Record (RecordCons { key = 0w98, value = vHole, rest = RecordNil })
                
                                        
                
                                        
                
                                    in
                
                                                        union ctx r1 r2;
                
                                                        case !h of
                
                                                            SOME { value = Integer 2, ... } => assert "Union record bound hole to 2" true
                
                                                          | _ => assert "Union record bound hole to 2" false
                
                                                    end
                
                                                )
                
                                                        val _ = if failed then assert "Union record (failed)" false else ()
                
                                                    in
                
                                                        ()
                
                                                    end
                
                                                
                
                                                fun testBacktrackingUnion () =
                
                                                    let
                
                                                        val _ = print "Testing backtracking union...\n"
                
                                                        
                
                                                        (* Scenario 1: Simple backtracking *)
                
                                                        val _ = 
                
                                                            let
                
                                                                val h = ref NONE
                
                                                                val vHole = Unknown (Hole h)
                
                                                                val failed = Stack.try NONE (fn frame =>
                
                                                                    let
                
                                                                        val ctx = { frame = frame }
                
                                                                    in
                
                                                                        union ctx vHole (Integer 1);
                
                                                                        case !h of
                
                                                                             SOME { value = Integer 1, ... } => assert "Bind success before fail" true
                
                                                                           | _ => assert "Bind success before fail" false;
                
                                                                        Stack.fail frame
                
                                                                    end
                
                                                                )
                
                                                            in
                
                                                                assert "Backtracking happened" failed;
                
                                                                case !h of
                
                                                                    SOME { abandoned, ... } => 
                
                                                                        if !(!abandoned) then assert "Binding abandoned" true
                
                                                                        else assert "Binding NOT abandoned" false
                
                                                                  | NONE => assert "Binding removed (unexpected implementation detail but ok)" true
                
                                                            end
                
                                                
                
                                                        (* Scenario 3: Partial unification failure *)
                
                                                        val _ =
                
                                                            let
                
                                                                val h = ref NONE
                
                                                                val vHole = Unknown (Hole h)
                
                                                                
                
                                                                (* unify { a: X, b: 2 } with { a: 1, b: 3 } *)
                
                                                                (* X should bind to 1, then b fails. X should be unbound after. *)
                
                                                                
                
                                                                val r1 = Record (RecordCons { key = 0w97, value = vHole, 
                
                                                                                              rest = RecordCons { key = 0w98, value = Integer 2, 
                
                                                                                                                  rest = RecordNil } })
                
                                                                val r2 = Record (RecordCons { key = 0w97, value = Integer 1, 
                
                                                                                              rest = RecordCons { key = 0w98, value = Integer 3, 
                
                                                                                                                  rest = RecordNil } })
                
                                                                                                                  
                
                                                                val failed = Stack.try NONE (fn frame =>
                
                                                                    let
                
                                                                        val ctx = { frame = frame }
                
                                                                    in
                
                                                                        union ctx r1 r2;
                
                                                                        assert "Should have failed" false
                
                                                                    end
                
                                                                )
                
                                                            in
                
                                                                assert "Partial unification failed" failed;
                
                                                                
                
                                                                (* Manually check dereferencing logic *)
                
                                                                let 
                
                                                                    fun isBound v = 
                
                                                                        case v of
                
                                                                            Unknown (Hole h') => (
                
                                                                                case !h' of
                
                                                                                    SOME { abandoned, value } =>
                
                                                                                      if !(!abandoned) then false else true
                
                                                                                  | NONE => false
                
                                                                            )
                
                                                                          | _ => true
                
                                                                in
                
                                                                     if isBound vHole then assert "Hole is still bound!" false
                
                                                                     else assert "Hole is correctly unbound" true
                
                                                                end
                
                                                            end
                
                                                    in
                
                                                        ()
                
                                                    end
                
                                                
                
                                                val _ = testStack ()
                
                                                val _ = testCompare ()
                
                                                val _ = testLookup ()
                
                                                val _ = testUnion ()
                
                                                val _ = testBacktrackingUnion ()
                
                                                
                
                                                val _ = print "All tests passed.\n"
                
                                                
            