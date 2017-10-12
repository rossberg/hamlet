(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure Char :> CHAR
  where type char = char
  where type string = string =
struct
  type char           = char
  type string         = string

  val ord             = use{b = "Char.ord"} : char -> int
  val chr             = use{b = "Char.chr"} : int -> char

  val maxOrd          = 255
  val minChar         = chr 0
  val maxChar         = chr maxOrd

  fun succ c          = if c = maxChar then raise Chr else chr(ord c + 1)
  fun pred c          = if c = minChar then raise Chr else chr(ord c - 1)

  val String_sub      = use{b = "String.sub"} : string * int -> char
  val String_size     = use{b = "String.size"} : string -> int
  val String_str      = use{b = "String.str"} : char -> string
  val op^             = use{b = "String.^"} : string * string -> string

  fun contains s c    = conts'(s, c, String_size s - 1)
  and conts'(s, c, i) = i >= 0 andalso
                        (String_sub(s, i) = c orelse conts'(s, c, i - 1))

  fun notContains s c = Bool.not(contains s c)

  fun isUpper c       = #"A" <= c andalso c <= #"Z"
  fun isLower c       = #"a" <= c andalso c <= #"z"
  fun isDigit c       = #"0" <= c andalso c <= #"9"
  fun isAlpha c       = isUpper c orelse isLower c
  fun isAlphaNum c    = isAlpha c orelse isDigit c
  fun isHexDigit c    = isDigit c orelse (#"a" <= c andalso c <= #"f")
                                  orelse (#"A" <= c andalso c <= #"F")
  fun isGraph c       = #"!" <= c andalso c <= #"~"
  fun isPrint c       = isGraph c orelse c = #" "
  fun isPunct c       = isGraph c andalso Bool.not(isAlphaNum c)
  fun isCntrl c       = Bool.not(isPrint c)
  fun isSpace c       = (#"\t" <= c andalso c <= #"\r") orelse c = #" "
  fun isAscii c       = 0 <= ord c andalso ord c <= 127

  fun toLower c       = if isUpper c then chr(ord c + 32) else c
  fun toUpper c       = if isLower c then chr(ord c - 32) else c

  fun toControl c     = "\\^" ^ String_str(chr(ord c + ord #"@"))
  fun toAscii c       = "\\" ^ String_str(chr(ord c div 100 + ord #"0"))
                             ^ String_str(chr(ord c mod 100 div 10 + ord #"0"))
                             ^ String_str(chr(ord c mod 10 + ord #"0"))
  fun toOctAscii c    = "\\" ^ String_str(chr(ord c div 64 + ord #"0"))
                             ^ String_str(chr(ord c mod 64 div 8 + ord #"0"))
                             ^ String_str(chr(ord c mod 8 + ord #"0"))

  fun toString #"\\"  = "\\\\"
    | toString #"\""  = "\\\""
    | toString #"\a"  = "\\a"
    | toString #"\b"  = "\\b"
    | toString #"\t"  = "\\t"
    | toString #"\n"  = "\\n"
    | toString #"\v"  = "\\v"
    | toString #"\f"  = "\\f"
    | toString #"\r"  = "\\r"
    | toString c      = if ord c < 32 then toControl c
                        else if ord c >= 127 then toAscii c
                        else String_str c

  fun toCString #"\\" = "\\\\"
    | toCString #"\"" = "\\\""
    | toCString #"?"  = "\\?"
    | toCString #"'"  = "\\'"
    | toCString #"\a" = "\\a"
    | toCString #"\b" = "\\b"
    | toCString #"\t" = "\\t"
    | toCString #"\n" = "\\n"
    | toCString #"\v" = "\\v"
    | toCString #"\f" = "\\f"
    | toCString #"\r" = "\\r"
    | toCString c     = if isPrint c then String_str c else toOctAscii c


  fun isOctDigit c = #"0" <= c andalso c <= #"7"

  fun value c = ord(toUpper c) - (if c < #"A" then ord #"0" else ord #"A" - 10)

  infix >>=
  fun NONE     >>= f = NONE
    | (SOME x) >>= f = f x

  fun scanAscii getc src0 =
      getc src0 >>= (fn(c1, src1) =>
      getc src1 >>= (fn(c2, src2) =>
      getc src2 >>= (fn(c3, src3) =>
        if List.all isDigit [c1, c2, c3] then
          let val i = 100*value c1 + 10*value c2 + value c3 in
            if i <= 255 then SOME(chr i, src3) else NONE
          end
        else NONE
      )))

  fun scanUnicode getc src0 =
      getc src0 >>= (fn(c1, src1) =>
      getc src1 >>= (fn(c2, src2) =>
      getc src2 >>= (fn(c3, src3) =>
      getc src3 >>= (fn(c4, src4) =>
        if List.all isHexDigit [c1, c2, c3, c4] then
          SOME(chr(4096*value c1 + 256*value c2 + 16*value c3 + value c4), src4)
            handle Chr => NONE
        else NONE
      ))))

  fun scanControl getc src =
      getc src >>= (fn(c, src') =>
        if 64 <= ord c andalso ord c < 96 then SOME(chr(ord c - 64), src') else
        NONE
      )

  fun scan getc src =
      scan' getc src >>= (fn(c, src') =>
      scanOptGap getc src' >>= (fn src'' =>
        SOME(c, src'')
      ))
  and scan' getc src =
      getc src >>= (fn(c, src') =>
        if c = #"\\" then scanEscape getc src' else
        if isPrint c then SOME(c, src') else
        NONE
      )

  and scanEscape getc src =
      getc src >>= (fn(c, src') =>
        if isDigit c then scanAscii getc src else
        if isSpace c then scanGap getc src' >>= scan' getc else
        case c of
          #"a"  => SOME(#"\a", src')
        | #"b"  => SOME(#"\b", src')
        | #"t"  => SOME(#"\t", src')
        | #"n"  => SOME(#"\n", src')
        | #"v"  => SOME(#"\v", src')
        | #"f"  => SOME(#"\f", src')
        | #"r"  => SOME(#"\r", src')
        | #"\\" => SOME(#"\\", src')
        | #"\"" => SOME(#"\"", src')
        | #"^"  => scanControl getc src'
        | #"u"  => scanUnicode getc src'
        | _     => NONE
      )

  and scanGap getc src =
      getc src >>= (fn(c, src') =>
        if c = #"\\" then SOME src' else
        if isSpace c then scanGap getc src' else
        NONE
      )

  and scanOptGap getc src = SOME(Option.getOpt(scanOptGap' getc src, src))
  and scanOptGap' getc src =
      getc src >>= (fn(c1, src') =>
      getc src' >>= (fn(c2, src'') =>
        if c1 = #"\\" andalso isSpace c2
        then scanGap getc src'' >>= scanOptGap getc
        else NONE
      ))


  fun scanCAscii getc src =
      scanCAscii' 0 0 getc src >>= (fn(i, k, src') =>
        if k = 0 then NONE else SOME(chr i, src') handle Chr => NONE
      )
  and scanCAscii' i 3 getc src = SOME(i, 3, src)
    | scanCAscii' i k getc src =
      case getc src of
        NONE => SOME(i, k, src)
      | SOME(c, src') =>
          if isOctDigit c
          then scanCAscii'(8*i + value c) (k + 1) getc src'
          else SOME(i, k, src)

  fun scanCUnicode getc src =
      (scanCUnicode' 0 0 getc src handle Overflow => NONE) >>=
        (fn(i, k, src') =>
          if k = 0 then NONE else SOME(chr i, src') handle Chr => NONE
        )
  and scanCUnicode' i k getc src =
      case getc src of
        NONE => SOME(i, k, src)
      | SOME(c, src') =>
          if isHexDigit c
          then scanCUnicode' (16*i + value c) (k + 1) getc src'
          else SOME(i, k, src)

  fun scanCEscape getc src =
      getc src >>= (fn(c, src') =>
        if isDigit c then scanCAscii getc src else
        case c of
          #"a"  => SOME(#"\a", src')
        | #"b"  => SOME(#"\b", src')
        | #"t"  => SOME(#"\t", src')
        | #"n"  => SOME(#"\n", src')
        | #"v"  => SOME(#"\v", src')
        | #"f"  => SOME(#"\f", src')
        | #"r"  => SOME(#"\r", src')
        | #"?"  => SOME(#"?",  src')
        | #"\\" => SOME(#"\\", src')
        | #"\"" => SOME(#"\"", src')
        | #"'"  => SOME(#"'", src')
        | #"^"  => scanControl getc src'
        | #"x"  => scanCUnicode getc src'
        | _     => NONE
      )

  fun scanC getc src =
      getc src >>= (fn(c, src') =>
        if c = #"\\" then scanCEscape getc src' else
        if isPrint c then SOME(c, src') else
        NONE
      )


  fun scanString f s = Option.map #1 (f (reader s) 0 : ('a * int) option)
  and reader s i     = SOME(String_sub(s, i), i + 1) handle Subscript => NONE

  val fromString  = scanString scan
  val fromCString = scanString scanC


  val op<  = op<  : (char * char) -> bool
  val op<= = op<= : (char * char) -> bool
  val op>  = op>  : (char * char) -> bool
  val op>= = op>= : (char * char) -> bool

  fun compare(c1, c2) =
      if c1 < c2 then LESS else if c1 = c2 then EQUAL else GREATER
end;
