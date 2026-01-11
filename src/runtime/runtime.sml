structure Cont = MLton.Cont

datatype Value =
         Integer of int
         | Real of real
         | String of string
         | Symbol of word
         | Record of Record
         | Unknown of Value Hole
     and Record =
         RecordCons of { key : word, value: Value, rest: Record }
         | RecordNil
         | RecordHole of Record Hole
     and 'a Hole = Hole of { abandoned : bool ref ref, value : 'a } option ref

structure Stack = struct

datatype Frame = Frame of {
             return : unit Cont.t,
             failed : bool ref ref
         }

fun fail (Frame { return, failed }) = (
    ! failed := true;
    Cont.throw (return, ())
)

fun try (parent: Frame option) (f: Frame -> unit) =
    let val failed = ref (ref false); in (
        Cont.callcc (
            fn k => let val frame = Frame { return = k, failed }
                  in (f frame;
                      failed := (case parent of
                                     SOME (Frame { failed = failedParent, ... }) => ! failedParent
                                   | _ => (ref false)))
                  end
        );
        ! (! failed)
    )
    end

end (* structure Stack *)

type Context = { frame: Stack.Frame }

fun compare ({ frame }: Context) (a: Value) (b: Value) =
    case a of
        Integer x => (
         case b of
             Integer y => Int.compare (x, y)
           | Real y => Real.compare (Real.fromInt x, y)
           | _ => GREATER
     )
      | Real x => (
          case b of
              Integer y => Real.compare (x, Real.fromInt y)
            | Real y => Real.compare (x, y)
            | _ => GREATER
      )
      | String x => (
          case b of
              Integer _ => LESS
            | Real _ => LESS
            | String y => String.compare (x, y)
            | _ => GREATER
      )
      | Symbol x => (
          case b of
              Integer _ => LESS
            | Real _ => LESS
            | String _ => LESS
            | Symbol y => Word.compare (x, y)
            | _ => GREATER
      )
      | _ => Stack.fail frame

fun lookup ({ frame } : Context) (needle: word) (record: Record) =
    case record of
        RecordCons { key, value, rest } => if needle = key then SOME value else lookup { frame } needle rest
      | RecordNil => NONE
      | RecordHole (Hole hole) =>
        let val Stack.Frame { failed, ... } = frame in
        (case !hole of
             SOME { abandoned, value } =>
               if !(!abandoned)
               then (let
                   val valueHole = Hole (ref NONE)
                   val recordHole = Hole (ref NONE)
               in
                   hole := SOME { abandoned = failed, value = RecordCons { key = needle, value = Unknown valueHole, rest = RecordHole recordHole }};
                   SOME (Unknown valueHole)
               end)
               else lookup { frame } needle value
           | NONE =>
             let
                 val valueHole = Hole (ref NONE)
                 val recordHole = Hole (ref NONE)
             in
                 hole := SOME { abandoned = failed, value = RecordCons { key = needle, value = Unknown valueHole, rest = RecordHole recordHole }};
                 SOME (Unknown valueHole)
             end
        )
        end

fun union ({ frame }: Context) (a: Value) (b: Value) =
    let
        val Stack.Frame { failed, ... } = frame

        fun derefVal v =
            case v of
                Unknown (Hole h) => (
                    case !h of
                        SOME { abandoned, value } =>
                          if !(!abandoned) then v else derefVal value
                      | NONE => v
                )
              | _ => v

        fun derefRec r =
            case r of
                RecordHole (Hole h) => (
                    case !h of
                        SOME { abandoned, value } =>
                          if !(!abandoned) then r else derefRec value
                      | NONE => r
                )
              | _ => r

        fun bindVal (Hole h) v =
            h := SOME { abandoned = failed, value = v }

        fun bindRec (Hole h) r =
            h := SOME { abandoned = failed, value = r }

        fun unifyVal (v1, v2) =
            let
                val v1 = derefVal v1
                val v2 = derefVal v2
            in
                case (v1, v2) of
                    (Unknown (Hole h1), Unknown (Hole h2)) =>
                      if h1 = h2 then () else bindVal (Hole h1) v2
                  | (Unknown h, v) => bindVal h v
                  | (v, Unknown h) => bindVal h v
                  | (Record r1, Record r2) => unifyRec (r1, r2)
                  | (Integer i1, Integer i2) => if i1 = i2 then () else Stack.fail frame
                  | (Real x1, Real x2) => if Real.== (x1, x2) then () else Stack.fail frame
                  | (String s1, String s2) => if s1 = s2 then () else Stack.fail frame
                  | (Symbol s1, Symbol s2) => if s1 = s2 then () else Stack.fail frame
                  | _ => Stack.fail frame
            end

        and unifyRec (r1, r2) =
            let
                val r1 = derefRec r1
                val r2 = derefRec r2

                fun unifyFields (record, other) =
                    case record of
                        RecordCons { key, value, rest } =>
                        (case lookup { frame = frame } key other of
                             SOME value' => unifyVal (value, value')
                           | NONE => ();
                         unifyFields (rest, other))
                      | _ => ()
            in
                case (r1, r2) of
                    (RecordHole (Hole h1), RecordHole (Hole h2)) =>
                      if h1 = h2 then () else bindRec (Hole h1) r2
                  | (RecordHole h, r) => bindRec h r
                  | (r, RecordHole h) => bindRec h r
                  | _ => (unifyFields (r1, r2); unifyFields (r2, r1))
            end
    in
        unifyVal (a, b)
    end

