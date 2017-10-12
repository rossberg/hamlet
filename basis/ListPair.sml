(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure ListPair :> LIST_PAIR =
struct
  exception UnequalLengths

  fun zip(x1::l1', x2::l2')          = (x1, x2)::zip(l1', l2')
    | zip( _,      _)                = nil

  fun zipEq(x1::l1', x2::l2')        = (x1, x2)::zipEq(l1', l2')
    | zipEq(  nil,     nil  )        = nil
    | zipEq      _                   = raise UnequalLengths

  fun unzip l                        = unzip'(l, nil, nil)
  and unzip'((x1, x2)::l', l1, l2)   = unzip'(l', x1::l1, x2::l2)
    | unzip'(nil,          l1, l2)   = (List.rev l1, List.rev l2)

  fun app f (l1, l2)                 = List.app f (zip(l1, l2))
  fun map f (l1, l2)                 = List.map f (zip(l1, l2))

  fun appEq f (x1::l1', x2::l2')     = ( f(x1, x2); appEq f (l1', l2') )
    | appEq f (nil,     nil)         = ()
    | appEq f _                      = raise UnequalLengths

  fun mapEq f (x1::l1', x2::l2')     = f(x1, x2) :: mapEq f (l1', l2')
    | mapEq f (nil,     nil)         = nil
    | mapEq f _                      = raise UnequalLengths

  fun nonflat f                      = fn ((a, b), c) => f(a, b, c)
  fun foldl f c (l1, l2)             = List.foldl (nonflat f) c (zip(l1, l2))
  fun foldr f c (l1, l2)             = List.foldr (nonflat f) c (zip(l1, l2))

  fun foldlEq f c (x1::l1', x2::l2') = foldlEq f (f(x1, x2, c)) (l1', l2')
    | foldlEq f c (nil,     nil)     = c
    | foldlEq f c _                  = raise UnequalLengths

  fun foldrEq f c (x1::l1', x2::l2') = f(x1, x2, foldrEq f c (l1', l2'))
    | foldrEq f c (nil,     nil)     = c
    | foldrEq f c _                  = raise UnequalLengths

  fun all    f (l1, l2)              = List.all f (zip(l1, l2))
  fun exists f (l1, l2)              = List.exists f (zip(l1, l2))

  fun allEq f ([], [])               = true
    | allEq f (x::xs, y::ys)         = f(x, y) andalso allEq f (xs, ys)
    | allEq f _                      = false
end;
