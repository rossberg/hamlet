(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library
 *)

structure StringCvt :> STRING_CVT =
struct
    type cs			= int
    type ('a,'b) reader		= 'b -> ('a * 'b) option
    datatype radix		= BIN | OCT | DEC | HEX
    datatype realfmt		= SCI of int option
				| FIX of int option
				| GEN of int option
				| EXACT

    infix ^
    val op^			= String.^

    fun padLeft c i s		= let val n = i - String.size s in
				      if n <= 0 then s else
				      String.implode(List.tabulate(n, fn _=> c))
				      ^ s
				  end
    fun padRight c i s		= let val n = i - String.size s in
				      if n <= 0 then s else
				      s ^
				      String.implode(List.tabulate(n, fn _=> c))
				  end

    fun splitl p f src		= splitl'(p, f, src, nil)
    and splitl'(p, f, src, cs)	= case f src
 				    of NONE =>
				       (String.implode(List.rev cs), src)
				     | SOME (c, src') =>
				       if p c
				       then splitl'(p, f, src', c::cs)
				       else (String.implode(List.rev cs), src)

    fun takel p f s		= #1(splitl p f s)
    fun dropl p f s		= #2(splitl p f s)
    fun skipWS f s		= dropl Char.isSpace f s

    fun scanString f s		= Option.map #1
					     (f (reader s) 0 : ('a * cs) option)
    and reader s i		= SOME (String.sub(s, i), i+1)
				  handle Subscript => NONE
end;
