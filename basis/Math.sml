(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure Math :> MATH where type real = real =
struct
  type real = real

  val e     = use{b = "Math.e"} ()  : real
  val pi    = use{b = "Math.pi"} () : real

  val sqrt  = use{b = "Math.sqrt"}  : real -> real
  val sin   = use{b = "Math.sin"}   : real -> real
  val cos   = use{b = "Math.cos"}   : real -> real
  val tan   = use{b = "Math.tan"}   : real -> real
  val asin  = use{b = "Math.asin"}  : real -> real
  val acos  = use{b = "Math.acos"}  : real -> real
  val atan  = use{b = "Math.atan"}  : real -> real
  val atan2 = use{b = "Math.atan2"} : real * real -> real
  val exp   = use{b = "Math.exp"}   : real -> real
  val pow   = use{b = "Math.pow"}   : real * real -> real
  val ln    = use{b = "Math.ln"}    : real -> real
  val log10 = use{b = "Math.log10"} : real -> real
  val sinh  = use{b = "Math.sinh"}  : real -> real
  val cosh  = use{b = "Math.cosh"}  : real -> real
  val tanh  = use{b = "Math.tanh"}  : real -> real
end;
