(* String.fromString is broken in SML/NJ and Moscow ML,
 * and we stumble over it when bootstrapping the parser. *)

structure String =
struct
    open String

    (* Copied from basis/String.sml *)

    local

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

    in
        val fromString		= scanString scan
    end
end
