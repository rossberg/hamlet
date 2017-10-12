(* Moscow ML lacks some Real stuff and infinity and / raises Div exception *)

(* Use the largest available real as 'almost inf'... *)

structure Real =
struct
  open Real

  (* Approximation for Moscow ML *)
  fun x/y            = Real./(x, y) handle Div =>
                         if x > 0.0
                         then 8.98846567431e307
                         else ~8.98846567431e307

  fun isNan x        = !=(x, x)
  fun isFinite x     = abs x <> 1.0/0.0
  fun isNormal x     = isFinite x andalso abs x <> 0.0

  fun signBit x      = Int.<(sign x, 0) orelse x = ~0.0
  fun copySign(x, y) = if signBit x = signBit y then x else ~x

  fun checkFloat x   = if isNan x then raise Div
                       else if isFinite x then x
                       else raise Overflow

  fun toString x     = (if signBit x then "~" else "") ^ Real.toString(abs x)
end

val op/ = Real./
