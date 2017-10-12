(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML special constants
 *
 * Definition, Section 2.2
 * + RFC: Extended literal syntax
 *)

signature SCON =
sig
    (* Import *)

    type TyName = TyName.TyName

    (* Type [Section 2.2] *)

    (* [RFC: Extended literal syntax] *)
    datatype base = DEC | HEX | BIN

    datatype SCon =				(* [scon] *)
	  INT    of base * string * TyName option ref
	| WORD   of base * string * TyName option ref
	| STRING of string * TyName option ref
	| CHAR   of string * TyName option ref
	| REAL   of string * TyName option ref

    (* Operations *)

    val toString :	SCon -> string
    val tyname :	SCon -> TyName option
end;
