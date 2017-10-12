(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library value representation
 *
 * Definition, Sections 6.2 and E.1; Standard Basis Specification
 *
 * Note:
 * - We have to separate this from the rest of the Library module in order
 *   to avoid cycles in our architecture.
 *)

signature LIBRARY_SVAL =
sig
  type TyName          = StaticObjectsCore.TyName

  datatype IntSVal     = INT of int
  datatype WordSVal    = WORD of word | WORD8 of Word8.word
  datatype RealSVal    = REAL of real
  datatype CharSVal    = CHAR of char
  datatype StringSVal  = STRING of string

  val tWord8           : TyName

  val wordToWord8      : word -> Word8.word
  val word8ToWord      : Word8.word -> word
  val word8ToWordX     : Word8.word -> word

  val intToString      : IntSVal -> string
  val wordToString     : WordSVal -> string
  val realToString     : RealSVal -> string
  val charToString     : CharSVal -> string
  val stringToString   : StringSVal -> string

  val intFromString    : SCon.base * string * TyName option -> IntSVal
  val wordFromString   : SCon.base * string * TyName option -> WordSVal
  val realFromString   : string * TyName option -> RealSVal
  val stringFromString : string * TyName option -> StringSVal
  val charFromString   : string * TyName option -> CharSVal

  val compareInt       : IntSVal * IntSVal -> order
  val compareWord      : WordSVal * WordSVal -> order
  val compareChar      : CharSVal * CharSVal -> order
  val compareString    : StringSVal * StringSVal -> order
end;
