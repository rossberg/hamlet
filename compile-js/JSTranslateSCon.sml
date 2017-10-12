(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of special constants into JavaScript.
 *)

structure JSTranslateSCon : JS_TRANSLATE_SCON =
struct
  (* Import *)

  type SCon = SyntaxCore.SCon

  type expr = JSSyntax.expr

  datatype phrase = datatype Annotation.phrase

  structure JS    = JSSyntax


  (* Helpers *)

  fun num s = String.map (fn #"~" => #"-" | c => c) s

  fun str s =
      let
        val buf = CharArray.array(2 * String.size s, #" ")
        fun get i = String.sub(s, i)
        fun put(i, c) = CharArray.update(buf, i, c)
        fun digit c = Char.ord c - Char.ord #"0"
        fun hexchar d =
            Char.chr(d + (if d < 10 then Char.ord #"0" else Char.ord #"a" - 10))
        fun convert(i, j) =
            if i = String.size s then
              j
            else if get i <> #"\\" then
              (put(j, get i); convert(i + 1, j + 1))
            else case get(i + 1) of
              #"a" =>
                convertHex(7, i + 2, j)
            | #"^" =>
                convertHex(Char.ord(get(i + 2)) - 64, i + 3, j)
            | c =>
              if Char.isSpace c then
                convertSpace(i + 2, j)
              else if Char.isDigit c then
                convertHex(
                  100*digit(c) + 10*digit(get(i + 2)) + digit(get(i + 3)),
                  i + 4, j
                )
              else
                (put(j, #"\\"); put(j + 1, c); convert(i + 2, j + 2))
        and convertHex(n, i, j) =
            ( put(j, #"\\"); put(j + 1, #"x");
              put(j + 2, hexchar(n div 16)); put(j + 3, hexchar(n mod 16));
              convert(i, j + 4)
            )
        and convertSpace(i, j) =
            if get i = #"\\" then
              convert(i + 1, j)
            else
              convertSpace(i + 1, j)
      in
        CharArraySlice.vector(CharArraySlice.slice(buf, 0, SOME(convert(0, 0))))
      end


  (* Special constants *)

  fun translateSCon(SCon.INT(SCon.DEC, s)@@_)  = JS.NumExpr(num s)
    | translateSCon(SCon.INT(SCon.HEX, s)@@_)  = JS.NumExpr("0x" ^ num s)
    | translateSCon(SCon.WORD(SCon.DEC, s)@@_) = JS.NumExpr(num s)
    | translateSCon(SCon.WORD(SCon.HEX, s)@@_) = JS.NumExpr("0x" ^ num s)
    | translateSCon(SCon.STRING(s)@@_)         = JS.StringExpr(str s)
    | translateSCon(SCon.CHAR(s)@@_)           = JS.StringExpr(str s)
    | translateSCon(SCon.REAL(s)@@_)           = JS.NumExpr(num s)
end;
