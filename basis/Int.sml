(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure Int :> INTEGER where type int = int =
struct
  type int           = int

  val precision      = use{b = "Int.precision"} () : int option
  val minInt         = use{b = "Int.minInt"} () : int option
  val maxInt         = use{b = "Int.maxInt"} () : int option

  fun toLarge i      = i
  fun fromLarge i    = i
  fun toInt i        = i
  fun fromInt i      = i

  val abs            = abs : int -> int
  val op~            = op~ : int -> int
  val op+            = op+ : int * int -> int
  val op-            = op- : int * int -> int
  val op*            = op* : int * int -> int
  val op div         = op div : int * int -> int
  val op mod         = op mod : int * int -> int
  val op quot        = use{b = "Int.quot"} : int * int -> int
  val op rem         = use{b = "Int.rem"} : int * int -> int

  fun min(i, j)      = if i < j then i else j
  fun max(i, j)      = if i > j then i else j
  fun sign 0         = 0
    | sign i         = if i > 0 then 1 else ~1
  fun sameSign(i, j) = sign i = sign j


  open StringCvt

  (* fmt and scan both use inverted signs to cope with minInt *)

  fun base BIN = 2
    | base OCT = 8
    | base DEC = 10
    | base HEX = 16

  fun isDigit BIN = (fn c => #"0" <= c andalso c <= #"1")
    | isDigit OCT = (fn c => #"0" <= c andalso c <= #"7")
    | isDigit DEC = Char.isDigit
    | isDigit HEX = Char.isHexDigit

  fun digit i =
      Char.chr(i + (if i < 10 then Char.ord #"0" else Char.ord #"A" - 10))

  fun value c =
      Char.ord(Char.toUpper c) -
        (if c < #"A" then Char.ord #"0" else Char.ord #"A" - 10)

  fun fmt radix 0 = "0"
    | fmt radix i =
        if i > 0 then fmt'(base radix, ~i, [])
        else String.^("~", fmt'(base radix, i, []))
  and fmt'(b, 0, cs) = String.implode cs
    | fmt'(b, i, cs) = fmt'(b, quot(i, b), digit(~(rem(i, b))) :: cs)


  infix >>=
  fun NONE     >>= f = NONE
    | (SOME x) >>= f = f x

  fun scanSign getc src =
      case getc src of
        SOME(#"-", src') => SOME(1, src')
      | SOME(#"~", src') => SOME(1, src')
      | SOME(#"+", src') => SOME(~1, src')
      | _                => SOME(~1, src)

  fun scanHexPrefix getc src =
      getc src  >>= (fn(c1, src1) =>
      getc src1 >>= (fn(c2, src2) =>
        if c1 = #"0" andalso (c2 = #"x" orelse c2 = #"X") then SOME src2 else
        NONE
      ))
  fun scanPrefix radix getc src =
      if Bool.not(radix = HEX) then SOME src else
      case scanHexPrefix getc src of
        SOME src' => SOME src'
      | NONE      => SOME src

  fun scanNum (isDigit, base) getc src =
      scanNum' (isDigit, base, 0, 0) getc src >>= (fn(i, k, src') =>
        if k > 0 then SOME(i, src') else NONE
      )
  and scanNum' (isDigit, base, i, k) getc src =
      case getc src of
        SOME(c, src') =>
          if isDigit c
          then scanNum' (isDigit, base, base*i - value c, k + 1) getc src'
          else SOME(i, k, src)
      | NONE => SOME(i, k, src)

  fun scan radix getc src =
      scanSign getc (skipWS getc src) >>= (fn(sign, src1) =>
      scanPrefix radix getc src1 >>= (fn src2 =>
        case scanNum (isDigit radix, base radix) getc src2 of
          SOME(num, src3) => SOME(sign*num, src3)
        | NONE => scanNum (isDigit radix, 0) getc src1
      ))

  val toString   = fmt StringCvt.DEC
  val fromString = StringCvt.scanString(scan StringCvt.DEC)

  val op>  = op>  : int * int -> bool
  val op>= = op>= : int * int -> bool
  val op<  = op<  : int * int -> bool
  val op<= = op<= : int * int -> bool

  fun compare(i, j) =
      if i < j then LESS else if i = j then EQUAL else GREATER
end;
