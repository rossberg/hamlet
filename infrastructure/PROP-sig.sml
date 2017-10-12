(*
 * (c) Andreas Rossberg 2013
 *
 * Property lists
 *)

signature PROP =
sig
  type nil
  type ('a, 'b) cons
  type 'a prop

  val none : nil
  val new : unit -> ('a, 'b) cons
  val hd  : ('a, 'b) cons -> 'a prop
  val tl  : ('a, ('b, 'c) cons) cons -> ('b, 'c) cons

  val has : 'a prop -> bool
  val try : 'a prop -> 'a option
  val get : 'a prop -> 'a (* raises Option *)
  val set : 'a prop * 'a -> unit
end;
