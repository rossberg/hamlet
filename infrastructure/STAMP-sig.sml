(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Stamp generator.
 *)

signature STAMP =
sig
  eqtype stamp

  val stamp    : unit  -> stamp
  val toString : stamp -> string

  val reset    : unit -> unit

  val compare  : stamp * stamp -> order
  val min      : stamp * stamp -> stamp
end;
