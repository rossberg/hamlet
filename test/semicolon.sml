(* semicolon.sml *)

(* Checks parsing of semicolons. *)

structure A = struct ;;;;;;;; end;
signature S = sig ;;;;;;;;;;; end;
val _ = let ;;;;;;;;;;; in () end;

;;;;;;;;;;;;;;;;
