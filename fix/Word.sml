(* For non-up-to-date systems *)

structure Word =
struct
  open Word

  fun ~w = 0w0-w
  val fromLarge = fromLargeWord
  val toLarge   = toLargeWord
  val toLargeX  = toLargeWordX
end

structure Word8 =
struct
  open Word8

  fun ~w = 0w0-w
  val fromLarge = fromLargeWord
  val toLarge   = toLargeWord
  val toLargeX  = toLargeWordX
end
