(* Char.scan is missing in Moscow ML and broken in SML/NJ (e.g. on "\\u0000") *)

structure Char =
struct
  open Char

  (* Copied from basis/Char.sml *)

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
            if Int.<=(i, 255) then SOME(chr i, src3) else NONE
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
        if Int.<=(64, ord c) andalso Int.<(ord c, 96)
        then SOME(chr(ord c - 64), src')
        else NONE
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
end

structure Dummy = struct end
