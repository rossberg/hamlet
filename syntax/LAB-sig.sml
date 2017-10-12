(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML label identifiers
 *
 * Definition, Section 2.4
 *)

signature LAB =
sig
    (* Type [Section 2.4] *)

    eqtype Lab					(* [lab] *)

    (* Operations *)

    val fromString :	string -> Lab
    val fromInt :	int    -> Lab
    val toString :	Lab    -> string

    val compare :	Lab * Lab -> order
end;
