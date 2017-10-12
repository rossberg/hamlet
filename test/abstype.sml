(* abstype.sml *)

(* Checks equality inferred for abstype environments. *)

(* HaMLet cannot deal with eq2. *)

abstype t = T with
  datatype u = U of t
  val eq = op=
end

fun eq1(t1, t2) = U t1 = U t2;
fun eq2(t1, t2 : t) = eq(t1, t2);
