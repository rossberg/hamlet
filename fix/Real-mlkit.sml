(* MLKit lacks some Real stuff *)

(* Use the largest available real as 'almost inf'... *)

structure Real =
struct
  open Real

  fun isNormal x = isFinite x andalso !=(abs x, 0.0)

  (* Approximation for ML Kit *)
  fun signBit x      = Int.<(sign x, 0)
  fun copySign(x, y) = if signBit x = signBit y then x else ~x

  fun checkFloat x   = if isNan x then raise Div
                       else if isFinite x then x
                       else raise Overflow

  (* Approximation for ML Kit *)
  fun ?=(x, y)       = isNan x orelse isNan y orelse Real.==(x, y)
end
