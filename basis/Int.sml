(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library
 *)

structure Int :> INTEGER where type int = int =
struct
    type int		= int

    val precision	= use{b="Int.precision"} () : int option
    val minInt		= use{b="Int.minInt"} () : int option
    val maxInt		= use{b="Int.maxInt"} () : int option

    fun toLarge i	= i
    fun fromLarge i	= i
    fun toInt i		= i
    fun fromInt i	= i

    val abs		= abs : int -> int
    val op~		= op~ : int -> int
    val op+		= op+ : int * int -> int
    val op-		= op- : int * int -> int
    val op*		= op* : int * int -> int
    val op div		= op div : int * int -> int
    val op mod		= op mod : int * int -> int
    val op quot		= use{b="Int.quot"} : int * int -> int
    val op rem		= use{b="Int.rem"} : int * int -> int

    fun min(i, j)	= if i < j then i else j
    fun max(i, j)	= if i > j then i else j
    fun sign 0		= 0
      | sign i		= if i > 0 then 1 else ~1
    fun sameSign(i, j)	= sign i = sign j


    open StringCvt

    (* fmt and scan both use inverted signs to cope with minInt! *)

    fun base BIN	= 2
      | base OCT	= 8
      | base DEC	= 10
      | base HEX	= 16

    fun toIsDigit BIN	= (fn c => #"0" <= c andalso c <= #"1")
      | toIsDigit OCT	= (fn c => #"0" <= c andalso c <= #"7")
      | toIsDigit DEC	= Char.isDigit
      | toIsDigit HEX	= Char.isHexDigit

    fun digit i		= Char.chr(i + (if i < 10 then Char.ord #"0"
						  else Char.ord #"A" - 10))
    fun value c		= Char.ord(Char.toUpper c) -
				  (if c < #"A" then Char.ord #"0"
					       else Char.ord #"A" - 10)

    fun fmt radix 0	= "0"
      | fmt radix i	= if i > 0 then fmt'(base radix, ~i, [])
				   else String.^("~", fmt'(base radix, i, []))
    and fmt'(b, 0, cs)	= String.implode cs
      | fmt'(b, i, cs)	= fmt'(b, quot(i, b), digit(~(rem(i, b))) :: cs)


    fun scanSign getc src =
	case getc src
	  of SOME(#"-", src') => (1, src')
	   | SOME(#"~", src') => (1, src')
	   | SOME(#"+", src') => (~1, src')
	   | _                => (~1, src)

    fun scanPrefix getc src =
	case getc src
	  of SOME(#"0", src') =>
	     (case getc src'
		of SOME(#"x", src'') => (true, src'')
		 | SOME(#"X", src'') => (true, src'')
		 | _                 => (false, src)
	     )
	   | _ => (false, src)

    fun scanNum (isDigit, b) getc src =
	case getc src
	  of SOME(c, _) =>
	     if isDigit c
	     then SOME(scanNum' (isDigit, b) getc src 0)
	     else NONE
	   | NONE => NONE
    and scanNum' (isDigit, b) getc src i =
	case getc src
	  of SOME(c, src') =>
	     if isDigit c
	     then scanNum' (isDigit, b) getc src' (b*i - value c)
	     else (i, src)
	   | NONE => (i, src)

    fun scan radix getc src =
	let
	    val       src1  = skipWS getc src
	    val (sign,src2) = scanSign getc src1
	    val (pref,src3) = if radix = HEX then scanPrefix getc src2
					     else (false, src2)
	in
	    case scanNum (toIsDigit radix, base radix) getc src3
	      of SOME(num, src4) => SOME(sign*num, src4)
	       | NONE => if pref then SOME(0, #2(Option.valOf(getc src2)))
				 else NONE
	end

    val toString	= fmt StringCvt.DEC
    val fromString	= StringCvt.scanString(scan StringCvt.DEC)

    val op>		= op>  : int * int -> bool
    val op>=		= op>= : int * int -> bool
    val op<		= op<  : int * int -> bool
    val op<=		= op<= : int * int -> bool
    fun compare(i, j)	= if i < j then LESS
			  else if i = j then EQUAL
			  else GREATER
end;
