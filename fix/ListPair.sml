(* Eq variants do not exist in older SML versions *)

structure ListPair =
struct
  open ListPair

  exception UnequalLengths

  fun zipEq(x::xs, y::ys)        = (x, y)::zipEq(xs, ys)
    | zipEq(nil,   nil)          = nil
    | zipEq _                    = raise UnequalLengths

  fun appEq f (x::xs, y::ys)     = ( f(x, y); appEq f (xs, ys) )
    | appEq f (nil,     nil)     = ()
    | appEq f _                  = raise UnequalLengths

  fun mapEq f (x::xs, y::ys)     = f(x, y)::mapEq f (xs, ys)
    | mapEq f (nil,     nil)     = nil
    | mapEq f _                  = raise UnequalLengths

  fun foldlEq f c (x::xs, y::ys) = foldlEq f (f(x, y, c)) (xs, ys)
    | foldlEq f c (nil,   nil)   = c
    | foldlEq f c _              = raise UnequalLengths

  fun foldrEq f c (x::xs, y::ys) = f(x, y, foldrEq f c (xs, ys))
    | foldrEq f c (nil,   nil)   = c
    | foldrEq f c _              = raise UnequalLengths

  fun allEq f ([], [])           = true
    | allEq f (x::xs, y::ys)     = f(x, y) andalso allEq f (xs, ys)
    | allEq f _                  = false
end
