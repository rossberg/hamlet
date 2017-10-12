(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML finite sets
 *
 * Definition, Section 4.2
 *
 * Note: This signature just extends the one available in the SML/NJ lib.
 *)

signature FIN_SET =
sig
  include ORD_SET

  exception NotFound

  val fromList : item list -> set

  val first    : set -> item option (* raises NotFound *)
  val disjoint : set * set -> bool
end;
