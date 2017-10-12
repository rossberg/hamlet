(* undetermined2.sml *)

(* Checks inference for non-generalised types (aka "free type variables"). *)

(* The intention of the Definition is to reject these, but due to a
   misconception in the formalisation they actually have to be accepted,
   as far as the formal rules are concerned. *)

val a = ref nil;
val _ = a := [1];

val a = ref nil;
structure X = struct end;
val _ = a := [1];

val a = ref nil;
signature S = sig end;
val _ = a := [1];
