(* tyvar-shadowing.sml *)

(* Checks that type variables may not be shadowed. *)

val 'a x = let val 'a y = () in () end;
