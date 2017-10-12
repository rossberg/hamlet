(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure StringCvt :> STRING_CVT =
struct
  type cs = int
  type ('a,'b) reader = 'b -> ('a * 'b) option

  datatype radix = BIN | OCT | DEC | HEX
  datatype realfmt =
      SCI of int option
    | FIX of int option
    | GEN of int option
    | EXACT

  infix ^
  val op^ = String.^

  fun clamp i = if i < 0 then 0 else i
  fun padding c i = String.implode(List.tabulate(clamp i, fn _ => c))
  fun padLeft  c i s = padding c (i - String.size s) ^ s
  fun padRight c i s = s ^ padding c (i - String.size s)

  fun splitl p f src = splitl'(p, f, src, nil)
  and splitl'(p, f, src, cs) =
      case f src of
        NONE =>
          (String.implode(List.rev cs), src)
      | SOME(c, src') =>
          if p c then splitl'(p, f, src', c::cs)
          else (String.implode(List.rev cs), src)

  fun takel p f s    = #1(splitl p f s)
  fun dropl p f s    = #2(splitl p f s)
  fun skipWS f s     = dropl Char.isSpace f s

  fun scanString f s = Option.map #1 (f (reader s) 0 : ('a * cs) option)
  and reader s i     = SOME(String.sub(s, i), i + 1) handle Subscript => NONE
end;
