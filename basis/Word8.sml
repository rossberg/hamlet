(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note:
 * - Dropped deprecated {from,to}LargeWord functions.
 *)

structure Word8 :> WORD where type word = Word8.word =
struct
  type word               = Word8.word

  val wordSize            = 8

  val toLarge             = use{b = "Word8.toLarge"} : word -> Word.word
  val toLargeX            = use{b = "Word8.toLargeX"}: word -> Word.word
  val fromLarge           = use{b = "Word8.fromLarge"}: Word.word ->word
  val toInt               = use{b = "Word8.toInt"} : word -> Int.int
  val toIntX              = use{b = "Word8.toIntX"} : word -> Int.int
  val fromInt             = use{b = "Word8.fromInt"} : Int.int -> word
  val toLargeInt          = toInt (* hm, what if Int <> LargeInt? *)
  val toLargeIntX         = toIntX
  val fromLargeInt        = fromInt

  val notb                = use{b = "Word8.notb"} : word -> word
  val orb                 = use{b = "Word8.orb"}  : word * word -> word
  val xorb                = use{b = "Word8.xorb"} : word * word -> word
  val andb                = use{b = "Word8.andb"} : word * word -> word
  val <<                  = use{b = "Word8.<<"}   : word * Word.word -> word
  val >>                  = use{b = "Word8.>>"}   : word * Word.word -> word
  val ~>>                 = use{b = "Word8.~>>"}  : word * Word.word -> word
  val op+                 = op+ : word * word -> word
  val op-                 = op- : word * word -> word
  val op*                 = op* : word * word -> word
  val op div              = op div : word * word -> word
  val op mod              = op mod : word * word -> word
  fun ~w                  = notb(w - 0w1)

  val op>                 = op>  : word * word -> bool
  val op>=                = op>= : word * word -> bool
  val op<                 = op<  : word * word -> bool
  val op<=                = op<= : word * word -> bool
  fun compare(i, j)       = if i < j then LESS else if i = j then EQUAL
                            else GREATER
  fun min(i, j)           = if i < j then i else j
  fun max(i, j)           = if i > j then i else j

  fun fmt radix i         = Word.fmt radix (toLarge i)
  fun scan radix getc src = case Word.scan radix getc src of
                              NONE => NONE
                            | SOME(w, src') =>
                                if Word.>(w, 0wxff) then raise Overflow else
                                SOME(fromLarge w, src')
  val toString            = fmt StringCvt.HEX
  val fromString          = StringCvt.scanString(scan StringCvt.HEX)
end;
