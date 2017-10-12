(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library
 *)

structure Char :> CHAR
    where type char = char
    where type string = string =
struct
    type char		= char
    type string		= string

    val ord		= use{b="Char.ord"} : char -> int
    val chr		= use{b="Char.chr"} : int -> char

    val maxOrd		= use{b="Char.maxOrd"} () : int
    val minChar		= chr 0
    val maxChar		= chr maxOrd

    fun succ c		= if c = maxChar then raise Chr else chr(ord c + 1)
    fun pred c		= if c = minChar then raise Chr else chr(ord c - 1)

    val String_sub	= use{b="String.sub"} : string * int -> char
    val String_size	= use{b="String.size"} : string -> int
    val String_str	= use{b="String.str"} : char -> string
    val op^		= use{b="String.^"} : string * string -> string

    fun contains s c	= conts'(s, c, String_size s - 1)
    and conts'(s, c, i)	= i >= 0 andalso
			  (String_sub(s, i) = c orelse conts'(s, c, i-1))

    fun notContains s c	= Bool.not(contains s c)

    fun isUpper c	= #"A" <= c andalso c <= #"Z"
    fun isLower c	= #"a" <= c andalso c <= #"z"
    fun isDigit c	= #"0" <= c andalso c <= #"9"
    fun isAlpha c	= isUpper c orelse isLower c
    fun isAlphaNum c	= isAlpha c orelse isDigit c
    fun isHexDigit c	= isDigit c orelse (#"a" <= c andalso c <= #"f")
				    orelse (#"A" <= c andalso c <= #"F")
    fun isGraph c	= #"!" <= c andalso c <= #"~"
    fun isPrint c	= isGraph c orelse c = #" "
    fun isPunct c	= isGraph c andalso Bool.not(isAlphaNum c)
    fun isCntrl c	= Bool.not(isPrint c)
    fun isSpace c    	= (#"\t" <= c andalso c <= #"\r") orelse c = #" "
    fun isAscii c	= 0 <= ord c andalso ord c <= 127

    fun toLower c	= if isUpper c then chr(ord c + 32) else c
    fun toUpper c	= if isLower c then chr(ord c - 32) else c


    fun toControl c	= "\\^" ^ String_str(chr(ord c + ord #"@"))
    fun toAscii c	= "\\" ^ String_str(chr(ord c div 100 + ord #"0"))
			      ^ String_str(chr(ord c mod 100 div 10 + ord #"0"))
			      ^ String_str(chr(ord c mod 10 + ord #"0"))
    fun toOctAscii c	= "\\" ^ String_str(chr(ord c div 64 + ord #"0"))
			      ^ String_str(chr(ord c mod 64 div 8 + ord #"0"))
			      ^ String_str(chr(ord c mod 8 + ord #"0"))

    fun toString #"\\"	= "\\\\"
      | toString #"\""	= "\\\""
      | toString #"\a"	= "\\a"
      | toString #"\b"	= "\\b"
      | toString #"\t"	= "\\t"
      | toString #"\n"	= "\\n"
      | toString #"\v"	= "\\v"
      | toString #"\f"	= "\\f"
      | toString #"\r"	= "\\r"
      | toString c	= if ord c < 32 then toControl c
			  else if ord c >= 127 then toAscii c
			  else String_str c

    fun toCString #"\\"	= "\\\\"
      | toCString #"\""	= "\\\""
      | toCString #"?"	= "\\?"
      | toCString #"'"	= "\\'"
      | toCString #"\a"	= "\\a"
      | toCString #"\b"	= "\\b"
      | toCString #"\t"	= "\\t"
      | toCString #"\n"	= "\\n"
      | toCString #"\v"	= "\\v"
      | toCString #"\f"	= "\\f"
      | toCString #"\r"	= "\\r"
      | toCString c	= if isPrint c then String_str c else toOctAscii c



    fun isOctDigit c	= #"0" <= c andalso c <= #"7"
    fun value c		= ord(toUpper c) - (if c < #"A" then ord #"0"
							else ord #"A" - 10)
    fun scanAscii getc src0 =
	case getc src0 of NONE => NONE | SOME(c1, src1) =>
	case getc src1 of NONE => NONE | SOME(c2, src2) =>
	case getc src2 of NONE => NONE | SOME(c3, src3) =>
	if List.all isDigit [c1,c2,c3]
	then let val i = 100*value c1 + 10*value c2 + value c3
	     in if i <= 255 then SOME(chr i, src3) else NONE end
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
	   | SOME(c, src') => if 64 <= ord c andalso ord c <= 95
			      then SOME(chr(ord c - 64), src')
			      else NONE

    fun scan getc src =
	case scan' getc src
	  of NONE          => NONE
	   | SOME(c, src') => scanTrailGap getc src' c
    and scan' getc src =
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
				  then case scanGap getc src'
					 of NONE       => NONE
					  | SOME src'' => scan' getc src''
				  else NONE
    and scanGap getc src =
	case getc src
	  of NONE              => NONE
	   | SOME(#"\\", src') => SOME src'
	   | SOME(  c,   src') => if isSpace c
				  then scanGap getc src'
				  else NONE
    and scanTrailGap getc src c =
	case getc src
	  of SOME(#"\\", src') =>
	     (case getc src'
		of SOME(c', src'') =>
		   if isSpace c'
		   then case scanGap getc src''
			  of NONE        => SOME(c, src)
			   | SOME src''' => scanTrailGap getc src''' c
		   else SOME(c, src)
		 | NONE           => SOME(c, src)
	     )
	   | _ => SOME(c, src)

    fun scanCAscii getc src      = scanCAscii' getc src 0 0
				   handle Chr => NONE
    and scanCAscii' getc src i 3 = SOME(chr i, src)
      | scanCAscii' getc src i k =
	case getc src
	  of NONE          => scanCAscii'' src i k
	   | SOME(c, src') => if isOctDigit c
			      then scanCAscii' getc src' (8*i + value c) (k+1)
			      else scanCAscii'' src i k
    and scanCAscii'' src i 0 = NONE
      | scanCAscii'' src i k = SOME(chr i, src)

    fun scanCUnicode getc src = scanCUnicode' getc src 0 0
				handle Chr => NONE | Overflow => NONE
    and scanCUnicode' getc src i k =
	case getc src
	  of NONE          => scanCUnicode'' src i k
	   | SOME(c, src') => if isHexDigit c
			      then scanCUnicode' getc src' (16*i + value c) (k+1)
			      else scanCUnicode'' src i k
    and scanCUnicode'' src i 0 = NONE
      | scanCUnicode'' src i k = SOME(chr i, src)

    fun scanCEscape getc src =
	case getc src
	  of NONE              => NONE
	   | SOME(#"a",  src') => SOME(#"\a", src')
	   | SOME(#"b",  src') => SOME(#"\b", src')
	   | SOME(#"t",  src') => SOME(#"\t", src')
	   | SOME(#"n",  src') => SOME(#"\n", src')
	   | SOME(#"v",  src') => SOME(#"\v", src')
	   | SOME(#"f",  src') => SOME(#"\f", src')
	   | SOME(#"r",  src') => SOME(#"\r", src')
	   | SOME(#"?",  src') => SOME(#"?",  src')
	   | SOME(#"\\", src') => SOME(#"\\", src')
	   | SOME(#"\"", src') => SOME(#"\"", src')
	   | SOME(#"'",  src') => SOME(#"'", src')
	   | SOME(#"^",  src') => scanControl getc src'
	   | SOME(#"x",  src') => scanCUnicode getc src'
	   | SOME(  c,   src') => if isDigit c
				  then scanCAscii getc src
				  else NONE
    fun scanC getc src =
	case getc src
	  of NONE              => NONE
	   | SOME(#"\\", src') => scanCEscape getc src'
	   | SOME(  c,   src') => if isPrint c
				  then SOME(c, src')
				  else NONE

    fun scanString f s	= Option.map #1 (f (reader s) 0 : ('a * int) option)
    and reader s i	= SOME (String_sub(s, i), i+1) handle Subscript => NONE

    val fromString	= scanString scan
    val fromCString	= scanString scanC


    val op<		= op<  : (char * char) -> bool
    val op<=		= op<= : (char * char) -> bool
    val op>		= op>  : (char * char) -> bool
    val op>=		= op>= : (char * char) -> bool

    fun compare(c1, c2)	= if c1 < c2 then LESS
			  else if c1 = c2 then EQUAL
			  else GREATER
end;
