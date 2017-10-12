(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML identifiers
 *
 * Definition, Section 2.4
 *
 * Note:
 *   This is a generic signature to represent all kinds of identifiers (except
 *   for labels and tyvars).
 *)

signature ID =
sig
    (* Type [Section 2.4] *)

    eqtype Id					(* [id] *)

    (* Operations *)

    val invent :	unit -> Id

    val fromString :	string -> Id
    val toString :	Id -> string

    val compare :	Id * Id -> order
end;
