(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML addresses
 *
 * Definition, Section 6.2
 *)

signature ADDR =
sig
  (* Type [Section 6.2] *)

  eqtype Addr                                   (* [a] *)


  (* Operations *)

  val addr    : unit -> Addr
  val compare : Addr * Addr -> order
end;
