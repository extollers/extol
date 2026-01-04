
structure Cont = MLton.Cont

datatype Value =
  Integer of int
| Real of real
| String of string
| Data of string * Value list
| Hole of { abandoned : bool ref, value : Value } ref

structure Stack = struct

  datatype Frame = Frame of {
    return : bool Cont.t,
    failed : bool ref,
    parent : Frame option
  }

  fun fail ({ return }: Frame) = Cont.throw (return, false)

  (* fun pop (stack : Stack) ({failed = isFailed}) = *)
  (*     let val Frame { return, failed } = !stack in *)
  (*       failed := isFailed ; *)
  (*       case parent of *)
  (*       | NONE => OS.Process.exit (if isFailed then OS.Process.failure else OS.Process.success) *)
  (*       | SOME next => stack := next ; *)
  (*       Cont.throw (return, NONE) *)
  (*     end *)

  fun try (parent: Frame option) (f: () => ()) =
      if Cont.callcc (
              fn k => let val stack := Frame { return = k, failed = ref false, parent }
                    in f stack; true)
      then 
      else
end
