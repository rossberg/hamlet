(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Notes:
 * - Incomplete
 * - toString is currently implemented as primitive (it shouldn't be).
 *)

structure Real :> REAL where type real = real =
struct
  type real                   = real
  structure Math              = Math
  structure IEEE              = IEEEReal

(* Not supported by all implementations:
  val radix                   = use{b = "Real.radix"} () : int
  val precision               = use{b = "Real.precision"} () : int

  val maxFinite               = use{b = "Real.maxFinite"} () : real
  val minPos                  = use{b = "Real.minPos"} () : real
  val minNormalPos            = use{b = "Real.minNormalPos"} () : real
*)
  val posInf                  = 1.0/0.0
  val negInf                  = ~1.0/0.0

  fun toLarge x               = x
  fun fromLarge _ x           = x

  val fromInt                 = use{b = "Real.fromInt"} : Int.int -> real
  val floor                   = use{b = "Real.floor"}   : real -> Int.int
  val ceil                    = use{b = "Real.ceil"}    : real -> Int.int
  val trunc                   = use{b = "Real.trunc"}   : real -> Int.int
  val round                   = use{b = "Real.round"}   : real -> Int.int
(* Not supported by all implementations:
  val realFloor               = use{b = "Real.realFloor"} : real -> real
  val realCeil                = use{b = "Real.realCeil"}  : real -> real
  val realTrunc               = use{b = "Real.realTrunc"} : real -> real
  val realRound               = use{b = "Real.realRound"} : real -> real
*)

  fun toInt IEEE.TO_NEGINF    = floor
    | toInt IEEE.TO_POSINF    = ceil
    | toInt IEEE.TO_ZERO      = trunc
    | toInt IEEE.TO_NEAREST   = round

  val toLargeInt              = toInt
  val fromLargeInt            = fromInt
(*
  val toDecimal : real -> IEEEReal.decimal_approx
  val fromDecimal : IEEEReal.decimal_approx -> real
*)

  val abs                     = abs : real -> real
  val op~                     = op~ : real -> real
  val op+                     = op+ : real * real -> real
  val op-                     = op- : real * real -> real
  val op*                     = op* : real * real -> real
  val op/                     = op/ : real * real -> real
(* Not supported by all implementations:
  val rem                     = use{b = "Real.rem"} : real * real -> real
*)
  fun *+(x, y, z)             = x*y + z
  fun *-(x, y, z)             = x*y - z

  val op>                     = op>  : real * real -> bool
  val op>=                    = op>= : real * real -> bool
  val op<                     = op<  : real * real -> bool
  val op<=                    = op<= : real * real -> bool
  val op==                    = use{b = "Real.=="} : real * real -> bool
  val op?=                    = use{b = "Real.?="} : real * real -> bool
  val op!=                    = Bool.not o op==
  fun compareReal(i, j)       = if i < j then IEEE.LESS
                                else if ==(i, j) then IEEE.EQUAL
                                else if i > j then IEEE.GREATER
                                else IEEE.UNORDERED
  fun compare(i, j)           = case compareReal(i, j) of
                                  IEEE.LESS      => LESS
                                | IEEE.EQUAL     => EQUAL
                                | IEEE.GREATER   => GREATER
                                | IEEE.UNORDERED => raise IEEE.Unordered

  val isFinite                = use{b = "Real.isFinite"}   : real -> bool
  val isNan                   = use{b = "Real.isNan"}      : real -> bool
(* Not supported by all implementations:
  val isNormal                = use{b = "Real.isNormal"}   : real -> bool
*)
  val signBit                 = use{b = "Real.signBit"}    : real -> bool
  val checkFloat              = use{b = "Real.checkFloat"} : real -> real
  val copySign                = use{b = "Real.copySign"}   : real * real -> real
(* Not supported by all implementations:
  val nextAfter               = use{b = "Real.nextAfter"}  : real * real -> real
*)

  fun min(x, y)               = if x < y then x else y
  fun max(x, y)               = if x > y then x else y
  fun sign x                  = if ==(x, 0.0) then 0 else
                                if x > 0.0 then 1 else ~1
  fun sameSign(x, y)          = signBit x = signBit y
  fun unordered(x, y)         = isNan x orelse isNan y

(*
  fun class x                 = if isNan x then IEEE.NAN
                                else if Bool.not(isFinite x) then IEEE.INF
                                else if ==(x, 0.0) then IEEE.ZERO
                                else if isNormal x then IEEE.NORMAL
                                else IEEE.SUBNORMAL
*)

(*
     val toManExp : real -> {man : real, exp : int}
     val fromManExp : {man : real, exp : int} -> real
     val split : real -> {whole : real, frac : real}

     val fmt : StringCvt.realfmt -> real -> string
*)

  fun value c = fromInt(Int.-(Char.ord c, Char.ord #"0"))

  infix >>=
  fun NONE     >>= f = NONE
    | (SOME x) >>= f = f x

  fun scanSign getc src =
      case getc src of
        SOME(#"-", src') => SOME(~1.0, src')
      | SOME(#"~", src') => SOME(~1.0, src')
      | SOME(#"+", src') => SOME(1.0, src')
      | _                => SOME(1.0, src)

  fun scanTextual ss getc src =
      if Substring.size ss = 0 then SOME src else
      getc src >>= (fn(c, src') =>
        if Char.toLower c = Substring.sub(ss, 0)
        then scanTextual (Substring.triml 1 ss) getc src'
        else NONE
      )

  fun scanFractional getc src =
      getc src >>= (fn(c, src') =>
        if Char.isDigit c
        then SOME(scanFractional' 0.0 0.1 getc src)
        else NONE
      )
  and scanFractional' r d getc src =
      case getc src of
        SOME(c, src') =>
          if Char.isDigit c
          then scanFractional' (r + d * value c) (d/10.0) getc src'
          else (r, src)
      | NONE => (r, src)

  fun scanMantissa getc src =
      getc src >>= (fn(c, src') =>
        if c = #"." then scanFractional getc src' else
        if Char.isDigit c then SOME(scanMantissa' 0.0 getc src) else
        NONE
      )
  and scanMantissa' r getc src =
      case getc src of
        SOME(#".", src') =>
          (case scanFractional getc src' of
            SOME(r', src'') => (r + r', src'')
          | NONE => (r, src)
          )
      | SOME(c, src') =>
          if Char.isDigit c
          then scanMantissa' (10.0*r + value c) getc src'
          else (r, src)
      | NONE => (r, src)

  fun scanExp getc src =
      getc src >>= (fn(c, src') =>
        if c = #"e" orelse c = #"E" then scanExp' getc src' else NONE
      )
  and scanExp' getc src =
      scanSign getc src >>= (fn(sign, src1) =>
      getc src1 >>= (fn(c, _) =>
        if Char.isDigit c
        then scanExp'' 0.0 getc src1 >>= (fn(r, src2) => SOME(sign*r, src2))
        else NONE
      ))
  and scanExp'' exp getc src =
      case getc src of
        SOME(c, src') =>
          if Char.isDigit c
          then scanExp'' (10.0*exp + value c) getc src'
          else SOME(exp, src)
      | NONE => SOME(exp, src)

  fun scan getc src =
      scanSign getc (StringCvt.skipWS getc src) >>= (fn(sign, src1) =>
        case scanTextual (Substring.full "infinity") getc src1 of
          SOME src2 => SOME(sign*posInf, src2)
        | NONE =>
        case scanTextual (Substring.full "inf") getc src1 of
          SOME src2 => SOME(sign*posInf, src2)
        | NONE =>
        case scanTextual (Substring.full "nan") getc src1 of
          SOME src2 => SOME(0.0*posInf, src2)
        | NONE =>
        scanMantissa getc src1 >>= (fn(man, src2) =>
          case scanExp getc src2 of
            NONE => SOME(sign*man, src2)
          | SOME(exp, src3) =>
              SOME(
                if ==(man, 0.0) then 0.0 else sign*man*Math.pow(10.0, exp),
                src3
              )
        )
      )

  val toString   = use{b = "Real.toString"} : real -> string
  val fromString = StringCvt.scanString scan
end;
