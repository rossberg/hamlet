(* poly-exception.sml *)

(* Checks whether type variables are allowed with local exception declartions. *)

fun f x = let exception E of 'a in E end;
val x = let exception E of 'a in () end;
val _ = let exception E of 'a in E end;

local exception E of 'a in end;
structure S : sig end = struct exception E of 'a end;
