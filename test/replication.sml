(* replication.sml *)

(* Checks semantics of datatype replication. *)

type u = int
datatype v = datatype u

val A = 0 : v

structure X =
struct
  datatype t = A
end

structure Y =
struct
  type u = X.t
  datatype v = datatype u
  val n = A + 1
end

functor F(type t) =
struct
  datatype u = datatype t
  val A = A + 1
end

structure Z = F(datatype t = A);
