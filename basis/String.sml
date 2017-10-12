(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

structure String :> STRING
    where type string = string
    where type char = Char.char =
struct
    type string			= string
    type char			= Char.char

    val maxSize			= use{b="String.maxSize"} () : int
    val size			= use{b="String.size"} : string -> int
    val sub			= use{b="String.sub"} : string * int-> char
    val str			= use{b="String.str"} : char -> string
    val op^			= use{b="String.^"} : string * string -> string

    fun concat nil		= ""
      | concat(s::l)		= s ^ concat l

    fun concatWith s nil	= ""
      | concatWith s (s'::nil)	= s'
      | concatWith s (s'::l)	= s' ^ s ^ concatWith s l

    fun implode l		= concat(List.map str l)
    fun explode s		= explode'(s, size s - 1, nil)
    and explode'(s, i, l)	= if i < 0 then l else
				  explode'(s, i-1, sub(s, i)::l)

    fun substring(s, i, j)	= substring'(s, i, i-1+j, nil)
				  handle Overflow => raise Subscript
    and substring'(s, i, j, cs)	= if i > j then implode cs else
				  substring'(s, i, j-1, sub(s, j)::cs)

    fun extract(s, i, NONE)	= substring(s, i, size s - i)
      | extract(s, i, SOME j)	= substring(s, i, j)

    fun map f s			= implode(List.map f (explode s))
    fun translate f s		= concat(List.map f (explode s))

    fun fields  f  s		= fields'(f, s, 0, 0)
    and fields'(f, s, i, j)	= if j = size s then
				      fields''(s, i, j, nil)
				  else if f(sub(s, j)) then
				      fields''(s, i, j, fields'(f, s, j+1, j+1))
				  else
				      fields'(f, s, i, j+1)
    and fields''(s, i, j, ss)	= substring(s, i, j-i)::ss

    fun tokens  f  s		= tokens'(f, s, 0, 0)
    and tokens'(f, s, i, j)	= if j = size s then
				      tokens''(s, i, j, nil)
				  else if f(sub(s, j)) then
				      tokens''(s, i, j, tokens'(f, s, j+1, j+1))
				  else
				      tokens'(f, s, i, j+1)
    and tokens''(s, i, j, ss)	= if i = j then ss else substring(s, i, j-i)::ss

    fun isPrefix s1 s2		= isPrefix'(s1, s2, 0)
    and isPrefix'(s1, s2, i)	= i = size s1 orelse
				  i < size s2 andalso sub(s1, i) = sub(s2, i)
					      andalso isPrefix'(s1, s2, i+1)

    fun isSuffix s1 s2		= isSuffix'(s1, s2, 0)
    and isSuffix'(s1, s2, i)	= i = size s1 orelse
				  i < size s2 andalso
				  sub(s1, size s1-i-1) = sub(s2, size s2-i-1)
					      andalso isSuffix'(s1, s2, i+1)

    fun isSubstring s1 s2	= isSubstring'(s1, s2, 0)
    and isSubstring'(s1, s2, i)	= i + size s1 < size s2 andalso
				  (isSubstring''(s1, s2, i, 0)
				  orelse isSubstring'(s1, s2, i+1))
    and isSubstring''(s1, s2, i, j)
				= j = size s1 orelse
				  sub(s1, i) = sub(s2, i+j) andalso
				  isSubstring''(s1, s2, i, j+1)

    fun collate  f (s, t)	= collate'(f, s, t, 0)
    and collate'(f, s, t, i)	= case (i = size s, i = size t)
				    of (true,  true ) => EQUAL
				     | (true,  false) => LESS
				     | (false, true ) => GREATER
				     | (false, false) =>
				  case f(sub(s, i), sub(t, i))
				    of EQUAL => collate'(f, s, t, i+1)
				     | other => other
    val compare			= collate Char.compare

    val op<			= op<  : string * string -> bool
    val op<=			= op<= : string * string -> bool
    val op>			= op>  : string * string -> bool
    val op>=			= op<= : string * string -> bool

    val toString		= translate Char.toString
    val toCString		= translate Char.toCString

    fun scan getc src		= scan' getc src [] false
    and scan' getc src cs b	= case Char.scan getc src
				    of SOME(c, src') =>
				       scan' getc src' (c::cs) true
				     | NONE => scanTrGap getc src cs b
    and scanTrGap getc src cs b	= case getc src
				    of SOME(#"\\", src') =>
				       (case scanGap getc src'
					  of SOME src'' =>
						scanTrGap getc src'' cs true
					   | NONE => scan'' src cs b
				       )
				     | SOME _ => scan'' src cs b
				     | NONE   => scan'' src cs true
    and scanGap getc src	= case getc src
				    of NONE              => NONE
				     | SOME(#"\\", src') => SOME src'
				     | SOME(c, src') =>
				       if Char.isSpace c
				       then scanGap getc src'
				       else NONE
    and scan'' src cs true	= SOME(implode(List.rev cs), src)
      | scan'' src cs false	= NONE

    fun scanString f s		= Option.map #1
				  (f (reader s) 0 : ('a * int) option)
    and reader s i		= SOME(sub(s, i), i+1) handle Subscript => NONE

    val fromString		= scanString scan

(*
    val fromCString : String.string -> string option
*)
end;
