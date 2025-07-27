(* fun-partial.sml *)

(* Checks for correct treatment of pattern matching in curried functions. *)

(* Example due to Stephen Weeks. *)

datatype t = A | B
fun f A () = ()
val _ = f B  (* should succeed *)
