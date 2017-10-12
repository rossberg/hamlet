(* functor-poly2.sml *)

(* Checks higher-order unification with functors. *)

(* THIS SHOULD PRODUCE A COMPILE TIME ERROR! *)

functor F(type t) = struct val id = (print "Hello\n"; fn x => x) end

structure A = F(type t = int)
structure B = F(type t = bool)

val a = A.id 3
val b = B.id ""

functor F(type t) = struct val id = (print "Hello\n"; fn x => x) end

structure A = F(type t = int) : sig val id : int -> int end
structure B = F(type t = bool) : sig val id : string -> string end
