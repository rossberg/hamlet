(* fun-case.sml *)

(* Checks for parsing of "fun"/"case" combinations. *)

(* HaMLet cannot deal with this example, currently *)

fun f(x::xs) = case xs of (x::xs) => 2 | nil => 1
  | f nil    = 0;
