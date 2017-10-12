(* abstype2.sml *)

(* Checks equality inferred for abstype environments. *)

(* THIS SHOULD PRODUCE A COMPILE TIME ERROR! *)

abstype t = T with end

fun eq(t1, t2 : t) = t1 = t2;
