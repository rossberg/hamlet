(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library
 *
 * Note:
 * - Dropped deprecated {from,to}LargeWord functions.
 *)

structure Word :> WORD where type word = word =
struct
    type word			= word

    val wordSize		= use{b="Word.wordSize"} () : int

    fun toLarge w		= w
    fun toLargeX w		= w
    fun fromLarge w		= w
    val toInt			= use{b="Word.toInt"} : word -> Int.int
    val toIntX			= use{b="Word.toIntX"} : word -> Int.int
    val fromInt			= use{b="Word.fromInt"} : Int.int -> word
    val toLargeInt		= toInt (* mh, if Int <> LargeInt? *)
    val toLargeIntX		= toIntX
    val fromLargeInt		= fromInt

    open StringCvt

    (* fmt and scan both use inverted signs to cope with minInt! *)

    fun base BIN	= 0w2
      | base OCT	= 0w8
      | base DEC	= 0w10
      | base HEX	= 0w16

    fun toIsDigit BIN	= (fn c => #"0" <= c andalso c <= #"1")
      | toIsDigit OCT	= (fn c => #"0" <= c andalso c <= #"7")
      | toIsDigit DEC	= Char.isDigit
      | toIsDigit HEX	= Char.isHexDigit

    fun digit i		= Char.chr(if i < 10 then Char.ord #"0" + i
					     else Char.ord #"A" + i - 10)
    fun value c		= Char.ord(Char.toUpper c) -
				  (if c < #"A" then Char.ord #"0"
					       else Char.ord #"A" - 10)

    fun fmt radix 0w0	= "0"
      | fmt radix i	= fmt'(base radix, i, [])
    and fmt'(b, 0w0,cs)	= String.implode cs
      | fmt'(b, i, cs)	= fmt'(b, i div b, digit(toInt(i mod b)) :: cs)


    fun scanPrefix getc src =
	case getc src
	  of SOME(#"0", src') =>
	     (case getc src'
		of SOME(#"w", src'') => (true, src'')
		 | _                 => (false, src)
	     )
	   | _ => (false, src)

    fun scanHexPrefix getc src =
	case getc src
	  of SOME(#"0", src') =>
	     (case getc src'
		of SOME(#"x", src'') => (true, src'')
		 | SOME(#"X", src'') => (true, src'')
		 | SOME(#"w", src'') =>
		   (case getc src''
		      of SOME(#"x", src''') => (true, src''')
		       | SOME(#"X", src''') => (true, src''')
		       | _                  => (false, src)
		   )
		 | _                 => (false, src)
	     )
	   | _ => (false, src)

    fun scanNum (isDigit, b) getc src =
	case getc src
	  of SOME(c, _) =>
	     if isDigit c
	     then SOME(scanNum' (isDigit, b) getc src 0w0)
	     else NONE
	   | NONE => NONE
    and scanNum' (isDigit, b) getc src i =
	case getc src
	  of SOME(c, src') =>
	     if isDigit c
	     then if i > fromInt ~1 div b then raise Overflow
		  else scanNum' (isDigit, b) getc src' (b*i + fromInt(value c))
	     else (i, src)
	   | NONE => (i, src)

    fun scan radix getc src =
	let
	    val       src1  = skipWS getc src
	    val (pref,src2) = if radix = HEX then scanHexPrefix getc src1
					     else scanPrefix getc src1
	in
	    case scanNum (toIsDigit radix, base radix) getc src2
	      of NONE => if pref then SOME(0w0, #2(Option.valOf(getc src1)))
				 else NONE
	       | some => some
	end

    val toString	= fmt StringCvt.HEX
    val fromString	= StringCvt.scanString(scan StringCvt.HEX)


    val notb		= use{b="Word.notb"} : word -> word
    val orb		= use{b="Word.orb"}  : word * word -> word
    val xorb		= use{b="Word.xorb"} : word * word -> word
    val andb		= use{b="Word.andb"} : word * word -> word
    val <<		= use{b="Word.<<"}   : word * word -> word
    val >>		= use{b="Word.>>"}   : word * word -> word
    val ~>>		= use{b="Word.~>>"}  : word * word -> word
    val op+		= op+ : word * word -> word
    val op-		= op- : word * word -> word
    val op*		= op* : word * word -> word
    val op div		= op div : word * word -> word
    val op mod		= op mod : word * word -> word
    fun ~w		= notb(w-0w1)

    val op>		= op>  : word * word -> bool
    val op>=		= op>= : word * word -> bool
    val op<		= op<  : word * word -> bool
    val op<=		= op<= : word * word -> bool
    fun compare(i, j)	= if i < j then LESS
			  else if i = j then EQUAL
			  else GREATER
    fun min(i, j)	= if i < j then i else j
    fun max(i, j)	= if i > j then i else j
end;
