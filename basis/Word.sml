(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note:
 * - Dropped deprecated {from,to}LargeWord functions.
 *)

structure Word :> WORD where type word = word =
struct
  type word        = word

  val wordSize     = use{b = "Word.wordSize"} () : int

  fun toLarge w    = w
  fun toLargeX w   = w
  fun fromLarge w  = w
  val toInt        = use{b = "Word.toInt"} : word -> Int.int
  val toIntX       = use{b = "Word.toIntX"} : word -> Int.int
  val fromInt      = use{b = "Word.fromInt"} : Int.int -> word
  val toLargeInt   = toInt (* mh, if Int <> LargeInt? *)
  val toLargeIntX  = toIntX
  val fromLargeInt = fromInt

  open StringCvt

  fun base BIN = 0w2
    | base OCT = 0w8
    | base DEC = 0w10
    | base HEX = 0w16

  fun isDigit BIN = (fn c => #"0" <= c andalso c <= #"1")
    | isDigit OCT = (fn c => #"0" <= c andalso c <= #"7")
    | isDigit DEC = Char.isDigit
    | isDigit HEX = Char.isHexDigit

  fun digit i =
      Char.chr(if i < 10 then Char.ord #"0" + i else Char.ord #"A" + i - 10)

  fun value c =
      Char.ord(Char.toUpper c) -
        (if c < #"A" then Char.ord #"0" else Char.ord #"A" - 10)

  fun fmt radix 0w0    = "0"
    | fmt radix i      = fmt'(base radix, i, [])
  and fmt'(b, 0w0, cs) = String.implode cs
    | fmt'(b, i, cs)   = fmt'(b, i div b, digit(toInt(i mod b)) :: cs)


  infix >>=
  fun NONE     >>= f = NONE
    | (SOME x) >>= f = f x

  fun scanPrefix radix getc src =
      getc src  >>= (fn(c1, src1) =>
      getc src1 >>= (fn(c2, src2) =>
        if Bool.not(c1 = #"0") then NONE else
        if radix = HEX andalso (c2 = #"x" orelse c2 = #"X") then SOME src2 else
        if Bool.not(c2 = #"w") then NONE else
        getc src2 >>= (fn(c3, src3) =>
          SOME(if c3 = #"x" orelse c3 = #"X" then src3 else src2)
        )
      ))
  fun scanOptPrefix radix getc src =
      case scanPrefix radix getc src of
        SOME src' => SOME src'
      | NONE      => SOME src

  fun scanNum (isDigit, base) getc src =
      scanNum' (isDigit, base, 0w0, 0) getc src >>= (fn(w, k, src') =>
        if k > 0 then SOME(w, src') else NONE
      )
  and scanNum' (isDigit, base, w, k) getc src =
      case getc src of
        SOME(c, src') =>
          if Bool.not(isDigit c) then SOME(w, k, src) else
          if w > ~(0w1) div base then raise Overflow else
          scanNum' (isDigit, base, base*w + fromInt(value c), k + 1) getc src'
      | NONE => SOME(w, k, src)

  fun scan radix getc src =
      scanOptPrefix radix getc (skipWS getc src) >>= (fn src1 =>
        case scanNum (isDigit radix, base radix) getc src1 of
          SOME(num, src2) => SOME(num, src2)
        | NONE => scanNum (isDigit radix, 0w0) getc src
      )

  val toString   = fmt StringCvt.HEX
  val fromString = StringCvt.scanString(scan StringCvt.HEX)


  val notb   = use{b = "Word.notb"} : word -> word
  val orb    = use{b = "Word.orb"}  : word * word -> word
  val xorb   = use{b = "Word.xorb"} : word * word -> word
  val andb   = use{b = "Word.andb"} : word * word -> word
  val <<     = use{b = "Word.<<"}   : word * word -> word
  val >>     = use{b = "Word.>>"}   : word * word -> word
  val ~>>    = use{b = "Word.~>>"}  : word * word -> word
  val op+    = op+ : word * word -> word
  val op-    = op- : word * word -> word
  val op*    = op* : word * word -> word
  val op div = op div : word * word -> word
  val op mod = op mod : word * word -> word
  fun ~w     = notb(w - 0w1)

  val op>    = op>  : word * word -> bool
  val op>=   = op>= : word * word -> bool
  val op<    = op<  : word * word -> bool
  val op<=   = op<= : word * word -> bool

  fun compare(i, j) =
      if i < j then LESS else if i = j then EQUAL else GREATER

  fun min(i, j) = if i < j then i else j
  fun max(i, j) = if i > j then i else j
end;
