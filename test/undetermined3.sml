(* undetermined3.sml *)

(* Checks that matching undetermined types does not perform unsound unification.
   Example due to Stephen Weeks.
*)

(* HAS TO BE REJECTED! *)

signature S = sig val f : 'a -> 'a option end
structure X : S =
struct
  fun g() = let val r = ref NONE in fn x => !r before r := SOME x end
  val f = g()
end;
