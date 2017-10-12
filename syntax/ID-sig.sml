(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML identifiers
 *
 * Definition, Section 2.4
 *
 * Note:
 *   This is a generic signature to represent all kinds of identifiers (except
 *   labels and tyvars).
 *)

signature ID =
sig
  (* Type [Section 2.4] *)

  eqtype Id                                     (* [id] *)


  (* Operations *)

  val invent     : unit -> Id
  val isInvented : Id -> bool
  val fromString : string -> Id
  val toString   : Id -> string

  val compare    : Id * Id -> order
end;
