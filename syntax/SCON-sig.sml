(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML special constants
 *
 * Definition, Section 2.2
 *)

signature SCON =
sig
  (* Types [Section 2.2] *)

  datatype base = DEC | HEX

  datatype SCon =                               (* [scon] *)
      INT    of base * string
    | WORD   of base * string
    | STRING of string
    | CHAR   of string
    | REAL   of string


  (* Operations *)

  val toString : SCon -> string
end;
