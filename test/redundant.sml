(* redundant.sml *)

(* Checks detection of redundant matches involving exception constructors. *)

(* HaMLet does not handle it. *)

exception E
exception F = E

fun f E = 1
  | f F = 2
  | f _ = 0;

signature S = sig exception E end

structure X1 = struct exception E = E end
structure X2 : S = struct exception E = E end
structure X3 :> S = struct exception E = E end

fun f() = (raise E) handle E => () | X1.E => ()
fun f() = (raise E) handle E => () | X2.E => ()
fun f() = (raise E) handle E => () | X3.E => ()

functor F(exception E exception F) =
struct
  fun f() = (raise E) handle E => () | F => ()
end

structure X = F(exception E exception F = E)  (* should warn *)
structure X = F(exception E exception F = E)  (* should warn *)
structure X = F(exception E exception F);     (* should not warn *)
