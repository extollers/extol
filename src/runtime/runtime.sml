structure Cont = MLton.Cont

datatype Value =
         Integer of int
         | Real of real
         | String of string
         | Symbol of word
         | Record of Record
         | Unknown of Value Hole
     and Record =
         RecordCons of { key : word, value: Value, rest: (''a * 'b) Record }
         | RecordNil of unit
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
                                   | _ => (ref true)))
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
             Integer y => Int.compare x y
           | Real y => Real.compare (Real.fromInt x) y
           | _ => GREATER
     )
      | Real x => (
          case b of
              Integer y => Real.compare x (Real.fromInt y)
            | Real y => Real.compare x y
            | _ => GREATER
      | String x => (
          case b of
              Integer _ => LESSER
            | Real y => LESSER
            | String y => String.compare x y
            | _ => GREATER
      | Symbol x => (
          case b of
              Integer _ => LESSER
            | Real _ => LESSER
            | String _ => LESSER
            | Symbol y => Word.compare x y
            | _ => GREATER
      | _ => Stack.fail frame

fun lookup ({ frame } : Context) (needle: word) (record: Record) =
    case record of
        Cons { key, value, rest } => if needle = key then SOME value else lookup needle rest
      | Nil => None
      | Unknown (Hole hole) =>
        (case (! hole) of
            NONE => (let
             result = Hole NONE,
             new = ref (SOME { abandoned = #failed frame, value = Hole NONE })
         in (hole := new)))
