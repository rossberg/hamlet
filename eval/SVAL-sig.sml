(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML special values
 *
 * Definition, Section 6.2
 *)

signature SVAL =
sig
  (* Type [Section 6.2] *)

  datatype SVal =                               (* [sv] *)
      INT    of LibrarySVal.IntSVal
    | WORD   of LibrarySVal.WordSVal
    | STRING of LibrarySVal.StringSVal
    | CHAR   of LibrarySVal.CharSVal
    | REAL   of LibrarySVal.RealSVal

  (* Operations *)

  val toString : SVal -> string
  val equal    : SVal * SVal -> bool  (* raises Domain *)
  val compare  : SVal * SVal -> order (* raises Domain *)
end;
