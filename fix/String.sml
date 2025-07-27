(* String.fromString is broken in SML/NJ and Moscow ML,
 * and we stumble over it when bootstrapping the parser.
 * Also, Moscow ML does not provide String.concatWith. *)

structure String =
struct
  open String

  (* Copied from basis/String.sml *)

  local
    infix >>=
    fun NONE     >>= f = NONE
      | (SOME x) >>= f = f x

    fun scan getc src =
        scan' [] getc src >>= (fn(cs, src') =>
        scanOptGap getc src' >>= (fn src'' =>
          SOME(implode(List.rev cs), src'')
        ))
    and scan' cs getc src =
        case Char.scan getc src of
          SOME(c, src') => scan' (c::cs) getc src'
        | NONE => SOME(cs, src)

    and scanGap getc src =
        getc src >>= (fn(c, src') =>
          if c = #"\\" then SOME src' else
          if Char.isSpace c then scanGap getc src' else
         NONE
        ) 

    and scanOptGap getc src = SOME(Option.getOpt(scanOptGap' getc src, src))
    and scanOptGap' getc src =
        getc src >>= (fn(c1, src') =>
        getc src' >>= (fn(c2, src'') =>
          if c1 = #"\\" andalso Char.isSpace c2
          then scanGap getc src'' >>= scanOptGap getc
          else NONE
        ))

    fun scanString f s = Option.map #1 (f (reader s) 0 : ('a * int) option)
    and reader s i     = SOME(sub(s, i), i + 1) handle Subscript => NONE
  in
    val fromString = scanString scan

    fun concatWith s nil        = ""
      | concatWith s (s'::nil)  = s'
      | concatWith s (s'::l)    = s' ^ s ^ concatWith s l
  end
end
