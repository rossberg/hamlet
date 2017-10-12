(* tyname.sml *)

(* Checks scoping of type names (should be rejected!). *)

let
  val r = ref nil
  datatype t = C
in
  r := [C]
end;
