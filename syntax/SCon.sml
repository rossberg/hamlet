(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML special constants
 *
 * Definition, Section 2.2
 * + RFC: Extended literal syntax
 *)

structure SCon :> SCON =
struct
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


    (* Conversions *)

    fun baseChar DEC               = ""
      | baseChar HEX               = "x"
      (* [RFC: Extended literal syntax] *)
      | baseChar BIN               = "b"

    fun toString(INT(base, s, _))  = if baseChar base = "" then s
				     else "0" ^ baseChar base ^ s
      | toString(WORD(base, s, _)) = "0w" ^ baseChar base ^ s
      | toString(STRING(s, _))     = "\""  ^ s ^ "\""
      | toString(CHAR(s, _))       = "#\"" ^ s ^ "\""
      | toString(REAL(s, _))       = s

    fun tyname(INT(_, _, r))       = !r
      | tyname(WORD(_, _, r))      = !r
      | tyname(STRING(_, r))       = !r
      | tyname(CHAR(_, r))         = !r
      | tyname(REAL(_, r))         = !r
end;
