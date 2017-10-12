(* Char.scan is missing in Moscow ML and broken in SML/NJ (e.g. on "\\u0000") *)

structure Char =
struct
    open Char

    (* Copied from basis/Char.sml *)

    fun isOctDigit c	= #"0" <= c andalso c <= #"7"
    fun value c		= ord(toUpper c) - (if c < #"A" then ord #"0"
							else ord #"A" - 10)
    fun scanAscii getc src0 =
	case getc src0 of NONE => NONE | SOME(c1, src1) =>
	case getc src1 of NONE => NONE | SOME(c2, src2) =>
	case getc src2 of NONE => NONE | SOME(c3, src3) =>
	if List.all isDigit [c1,c2,c3]
	then let val i = 100*value c1 + 10*value c2 + value c3
	     in if Int.<=(i, 255) then SOME(chr i, src3) else NONE end
	else NONE

    fun scanUnicode getc src0 =
	case getc src0 of NONE => NONE | SOME(c1, src1) =>
	case getc src1 of NONE => NONE | SOME(c2, src2) =>
	case getc src2 of NONE => NONE | SOME(c3, src3) =>
	case getc src3 of NONE => NONE | SOME(c4, src4) =>
	if Bool.not(List.all isHexDigit [c1,c2,c3,c4]) then NONE else
	SOME(chr(4096*value c1 + 256*value c2 + 16*value c3 + value c4), src4)
	handle Chr => NONE

    fun scanControl getc src =
	case getc src
	  of NONE          => NONE
	   | SOME(c, src') => if chr 64 <= c andalso c <= chr 95
			      then SOME(chr(ord c - 64), src')
			      else NONE
    fun scan getc src =
	case getc src
	  of NONE              => NONE
	   | SOME(#"\\", src') => scanEscape getc src'
	   | SOME(  c,   src') => if isPrint c
				  then SOME(c, src')
				  else NONE
    and scanEscape getc src =
	case getc src
	  of NONE              => NONE
	   | SOME(#"a",  src') => SOME(#"\a", src')
	   | SOME(#"b",  src') => SOME(#"\b", src')
	   | SOME(#"t",  src') => SOME(#"\t", src')
	   | SOME(#"n",  src') => SOME(#"\n", src')
	   | SOME(#"v",  src') => SOME(#"\v", src')
	   | SOME(#"f",  src') => SOME(#"\f", src')
	   | SOME(#"r",  src') => SOME(#"\r", src')
	   | SOME(#"\\", src') => SOME(#"\\", src')
	   | SOME(#"\"", src') => SOME(#"\"", src')
	   | SOME(#"^",  src') => scanControl getc src'
	   | SOME(#"u",  src') => scanUnicode getc src'
	   | SOME(  c,   src') => if isDigit c
				  then scanAscii getc src
				  else if isSpace c
				  then scanGap getc src'
				  else NONE
    and scanGap getc src =
	case getc src
	  of NONE              => NONE
	   | SOME(#"\\", src') => scan getc src'
	   | SOME(  c,   src') => if isSpace c
				  then scanGap getc src'
				  else NONE
end
