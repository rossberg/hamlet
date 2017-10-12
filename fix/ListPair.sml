(* Eq variants do not exist in older SML versions *)

structure ListPair =
struct
    open ListPair

    exception UnequalLengths

    fun zipEq(x1::l1', x2::l2')		= (x1,x2) :: zipEq(l1', l2')
      | zipEq(  nil,     nil  )		= nil
      | zipEq      _ 			= raise UnequalLengths

    fun appEq f (x1::l1', x2::l2')	= (f(x1,x2); appEq f (l1', l2'))
      | appEq f (  nil,     nil  )	= ()
      | appEq _       _			= raise UnequalLengths

    fun mapEq f (x1::l1', x2::l2')	= f(x1,x2) :: mapEq f (l1', l2')
      | mapEq f (  nil,     nil  )	= nil
      | mapEq _       _			= raise UnequalLengths

    fun foldlEq f c (x1::l1', x2::l2')	= foldlEq f (f(x1,x2,c)) (l1', l2')
      | foldlEq f c (  nil,     nil  )	= c
      | foldlEq _ _       _		= raise UnequalLengths

    fun foldrEq f c (x1::l1', x2::l2')	= f(x1, x2, foldrEq f c (l1', l2'))
      | foldrEq f c (  nil,     nil  )	= c
      | foldrEq _ _       _		= raise UnequalLengths

    fun allEq f ([], [])		= true
      | allEq f (x::xs, y::ys)		= f(x, y) andalso allEq f (xs, ys)
      | allEq _       _			= false
end
