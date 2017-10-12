(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML special values
 *
 * Definition, Section 6.2
 *)

structure SVal :> SVAL =
struct
  (* Type [Section 6.2] *)

  datatype SVal =                               (* [sv] *)
    INT    of LibrarySVal.IntSVal
  | WORD   of LibrarySVal.WordSVal
  | STRING of LibrarySVal.StringSVal
  | CHAR   of LibrarySVal.CharSVal
  | REAL   of LibrarySVal.RealSVal


  (* Conversions *)

  fun toString(INT i)    = LibrarySVal.intToString i
    | toString(WORD w)   = "0wx" ^ LibrarySVal.wordToString w
    | toString(STRING s) = "\"" ^ LibrarySVal.stringToString s ^ "\""
    | toString(CHAR c)   = "#\"" ^ LibrarySVal.charToString c ^ "\""
    | toString(REAL r)   = LibrarySVal.realToString r

  (* Comparison *)

  fun compare(INT i1, INT i2)       = LibrarySVal.compareInt(i1, i2)
    | compare(WORD w1, WORD w2)     = LibrarySVal.compareWord(w1, w2)
    | compare(STRING s1, STRING s2) = LibrarySVal.compareString(s1, s2)
    | compare(CHAR c1, CHAR c2)     = LibrarySVal.compareChar(c1, c2)
    | compare _                     = raise Domain

  fun equal(sv1, sv2) = compare(sv1, sv2) = EQUAL
end;
