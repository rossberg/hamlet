(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML special constants
 *
 * Definition, Section 2.2
 *)

structure SCon :> SCON =
struct
  (* Types [Section 2.2] *)

  datatype base = DEC | HEX

  datatype SCon =                               (* [scon] *)
      INT    of base * string
    | WORD   of base * string
    | STRING of string
    | CHAR   of string
    | REAL   of string


  (* Conversions *)

  fun toString(INT(base, s))  = if base = DEC then s else "0x" ^ s
    | toString(WORD(base, s)) = (if base = DEC then "0w" else "0wx") ^ s
    | toString(STRING(s))     = "\""  ^ s ^ "\""
    | toString(CHAR(s))       = "#\"" ^ s ^ "\""
    | toString(REAL(s))       = s
end;
