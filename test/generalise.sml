(* generalise.sml *)

(* Checks that types are properly generalised at value bindings. *)

val f = fn x => x
and r = ref 5
and g = fn x => x;

val 'a (r, _) = (ref 6, fn x : 'a => x);

f 3; f "3";
g 4; g "4";
