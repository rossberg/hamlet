(* functor-poly.sml *)

(* Checks higher-order unification with functors. *)

(* Example due to
 * Dreyer, Harper, Chkravarty, Keller, Modular Type Classes
 * Draft 2006
 *)

functor F(type t) = struct val id = (print "Hello\n"; fn x => x) end

structure A = F(type t = int)
structure B = F(type t = bool)

val a = A.id 3
val b = B.id true

functor G(type t) = struct val id = (print "Hello\n"; fn x => x) end

structure C = G(type t = int)
structure D = G(type t = bool)

val a = C.id 3
val b = D.id 3

functor F(type t) = struct val id = (print "Hello\n"; fn x => x) end

structure A = F(type t = int) : sig val id : int -> int end
structure B = F(type t = bool) : sig val id : bool -> bool end

functor G(type t) = struct val id = (print "Hello\n"; fn x => x) end

structure C = G(type t = int) : sig val id : int -> int end
structure D = G(type t = bool) : sig val id : int -> int end
