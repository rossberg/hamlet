(* dec-strdec.sml *)

(* Checks parsing of decs as strdecs. *)

(*
        HaMLet  SML/NJ  MosML   MLton   MLKit   Poly/ML MLWorks Alice
        1.2     110.52  2.01    041109  4.1.1   4.1.3   2.0     1.0

A       -       +       +       +       -       +       ?       -
B       -       -       +       +       -       +       ?       -
C       -       -       +       +       -       +       ?       -
B'      -       -       +       +       -       +       ?       -
B''     -       -       -       -       -       -       ?       -
C'      -       -       -       -       -       -       ?       -

Note that none of them has to be accepted, but B'' and C' must not be.
*)

(* A *)
val x =
let
  fun double x = x + x
  val y = double 2.0
in () end;

(* B *)
structure X =
struct
  fun double x = x + x
  val y = double 2.0
end;

(* C *)
fun double x = x + x
val y = double 2.0;

(* B' *)
structure X =
struct
  fun double x = x + x;
  val y = double 2.0
end;

(* B'' *)
structure X =
struct
  fun double x = x + x
  structure X = struct end
  val y = double 2.0
end;

(* C' *)
fun double x = x + x;
val y = double 2.0;
