(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure Option :> OPTION =
struct
  datatype option = datatype option

  exception Option

  fun getOpt(SOME v, a)     = v
    | getOpt(NONE,   a)     = a

  fun isSome(SOME v)        = true
    | isSome NONE           = false

  fun valOf(SOME v)         = v
    | valOf NONE            = raise Option

  fun filter f a            = if f a then SOME a else NONE

  fun join NONE             = NONE
    | join(SOME v)          = v

  fun app f  NONE           = ()
    | app f (SOME v)        = f v

  fun map f  NONE           = NONE
    | map f (SOME v)        = SOME(f v)

  fun mapPartial f          = join o (map f)
  fun compose (f, g)        = (map f) o g
  fun composePartial (f, g) = (mapPartial f) o g
end;
