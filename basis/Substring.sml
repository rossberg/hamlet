(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

structure Substring :> SUBSTRING
  where type string = String.string
  where type char = Char.char =
struct
  type substring              = string * int * int
  type char                   = Char.char
  type string                 = String.string

  fun base ss                 = ss : substring
  val string                  = String.substring o base
  val size                    = #3 o base
  fun sub((s, i, n), j)       = String.sub(s, i+j)

  fun substring(s, i, j)      = (if i < 0 orelse j < 0 orelse
                                   String.size s < i + j
                                 then raise Subscript
                                 else (s, i, j)
                                ) handle Overflow => raise Subscript
  fun extract(s, i, NONE)     = substring(s, i, String.size s - i)
    | extract(s, i, SOME j)   = substring(s, i, j)
  fun full s                  = substring(s, 0, String.size s)

  fun isEmpty(s, i, n)        = n = 0

  fun getc(s, i, 0)           = NONE
    | getc(s, i, n)           = SOME(String.sub(s, i), (s, i + 1, n - 1))
  val first                   = Option.map #1 o getc

  fun Int_min(i, j)           = if i < j then i else j
  fun Int_max(i, j)           = if i > j then i else j
  fun triml k (s, i, n)       = if k < 0 then raise Subscript else
                                (s, i + Int_min(k, n), Int_max(n - k, 0))
  fun trimr k (s, i, n)       = if k < 0 then raise Subscript else
                                (s, i, Int_max(n - k, 0))

  fun slice(ss, i, SOME m)    = slice'(ss, i, m)
    | slice(ss, i, NONE)      = slice'(ss, i, size ss - i)
  and slice'((s, i, n), j, m) = (if j + m > n then raise Subscript else
                                 substring(s, i + j, m)
                                ) handle Overflow => raise Subscript

  val concat                  = String.concat o (List.map string)
  fun concatWith s            = String.concatWith s o (List.map string)
  fun explode ss              = String.explode(string ss)
  fun translate f ss          = String.concat(List.map f (explode ss))
  fun isPrefix s ss           = String.isPrefix s (string ss)
  fun isSubstring s ss        = String.isSubstring s (string ss)
  fun isSuffix s ss           = String.isSuffix s (string ss)
  fun compare(ss, st)         = String.compare(string ss, string st)
  fun collate f (ss, st)      = String.collate f (string ss, string st)

  fun splitAt((s, i, n), j)   = if j < 0 orelse j > n then raise Subscript else
                                ((s, i, j), (s, i + j, n - j))

  fun splitl f (s, i, n) = splitl'(f, s, i, n, i)
  and splitl'(f, s, i, n, j) =
      if j = i + n then
        ((s, i, n), (s, j, 0))
      else if f(String.sub(s, j)) then
        splitl'(f, s, i, n, j + 1)
      else
        ((s, i, j - i), (s, j, n - (j - i)))

  fun splitr f (s, i, n) = splitr'(f, s, i, n, i + n - 1)
  and splitr'(f, s, i, n, j) =
      if j < i then
        ((s, i, 0), (s, i, n))
      else if f(String.sub(s, j)) then
        splitr'(f, s, i, n, j - 1)
      else
        ((s, i, j - i + 1), (s, j + 1, n - (j - i + 1)))

  fun takel p ss = #1(splitl p ss)
  fun dropl p ss = #2(splitl p ss)
  fun taker p ss = #2(splitr p ss)
  fun dropr p ss = #1(splitr p ss)

  fun position s' (s, i, n) =
      let
        val (s, k, n') = position' s' (s, i, n)
      in
        ((s, i, k - i), (s, k, n'))
      end
  and position' s' (s, i, n) =
      if String.size s' > n then (s, i + n, 0)
      else if isPrefix s' (s, i, n) then (s, i, n)
      else position' s' (triml 1 (s, i, n))

  fun span((s, i, n), (s', i', n')) =
      if Bool.not(s = s') orelse i' + n' < i
      then raise Span
      else (s, i, (i' + n') - i)

  fun fields f (s, i, n) = fields'(f, s, i, i, n)
  and fields'(f, s, i, j, n) =
      if j = i + n then
        (s, i, j - i)::nil
      else if f(String.sub(s, j)) then
        (s, i, j - i)::fields'(f, s, j + 1, j + 1, n - 1)
      else
        fields'(f, s, i, j + 1, n - 1)

  fun tokens f s = List.filter (fn(s, i, n) => Bool.not(n = 0)) (fields f s)

  fun foldl f a ss = List.foldl f a (explode ss)
  fun foldr f a ss = List.foldr f a (explode ss)
  fun app f ss     = List.app f (explode ss)
end

type substring = Substring.substring;
