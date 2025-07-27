(* Large types are missing in Moscow ML *)

structure Word =
struct
  open Word
  fun fromLarge i = i
  fun toLarge i   = i
end

structure Real =
struct
  open Real
  fun fromLarge _ r = r
  fun toLarge r     = r
end

structure LargeInt  = Int
structure LargeWord = Word
structure LargeReal = Real

structure IEEEReal =
struct
  datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
end
