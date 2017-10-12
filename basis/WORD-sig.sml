(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note:
 * - Dropped deprecated {from,to}LargeWord functions.
 *)

structure Word      = struct type word = word end
structure LargeWord = struct type word = word end

signature WORD =
sig
  eqtype word
  val wordSize : int
  val toLarge : word -> LargeWord.word
  val toLargeX : word -> LargeWord.word
  val fromLarge : LargeWord.word -> word
  val toLargeInt : word -> LargeInt.int
  val toLargeIntX : word -> LargeInt.int
  val fromLargeInt : LargeInt.int -> word
  val toInt : word -> Int.int
  val toIntX : word -> Int.int
  val fromInt : Int.int -> word
  val orb : word * word -> word
  val xorb : word * word -> word
  val andb : word * word -> word
  val notb : word -> word
  val << : word * Word.word -> word
  val >> : word * Word.word -> word
  val ~>> : word * Word.word -> word
  val + : word * word -> word
  val - : word * word -> word
  val * : word * word -> word
  val div : word * word -> word
  val mod : word * word -> word
  val ~ : word -> word
  val compare : word * word -> order
  val > : word * word -> bool
  val < : word * word -> bool
  val >= : word * word -> bool
  val <= : word * word -> bool
  val min : word * word -> word
  val max : word * word -> word
  val fmt : StringCvt.radix -> word -> string
  val toString : word -> string
  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
  val fromString : string -> word option
end;
