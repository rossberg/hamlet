(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML special values
 *
 * Definition, Section 6.2
 *)

signature SVAL =
sig
    (* Type [Section 6.2] *)

    datatype SVal =				(* [sv] *)
	  INT    of LibrarySVal.IntSVal
	| WORD   of LibrarySVal.WordSVal
	| STRING of LibrarySVal.StringSVal
	| CHAR   of LibrarySVal.CharSVal
	| REAL   of LibrarySVal.RealSVal

    (* Operations *)

    val toString :  SVal -> string
    val equal :     SVal * SVal -> bool		(* may raise Domain *)
    val compare :   SVal * SVal -> order	(* may raise Domain *)
end;
