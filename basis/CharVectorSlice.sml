(*
 * (c) Andreas Rossberg 2002-2013
 *
 * Standard ML Basis Library
 *)

structure CharVectorSlice :> MONO_VECTOR_SLICE
  where type slice = Substring.substring
  where type vector = String.string
  where type elem = Char.char =
struct
  type elem    = Char.char
  type vector  = CharVector.vector
  type slice   = Substring.substring

  val base     = Substring.base
  val length   = Substring.size
  val isEmpty  = Substring.isEmpty
  val sub      = Substring.sub
  val full     = Substring.full
  val slice    = Substring.extract
  val subslice = Substring.slice
  val vector   = Substring.string
  val concat   = Substring.concat
  val getItem  = Substring.getc
  val collate  = Substring.collate

  fun appi  f  sl          = appi'(f, sl, 0)
  and appi'(f, sl, i)      = if i = length sl then () else
                             ( f(i, sub(sl, i)); appi'(f, sl, i + 1) )

  fun foldli f init sl     = foldli'(f, init, sl, 0)
  and foldli'(f, x, sl, i) = if i = length sl then x else
                             foldli'(f, f(i, sub(sl, i), x), sl, i + 1)
  fun foldri f init sl     = foldri'(f, init, sl, length sl)
  and foldri'(f, x, sl, i) = if i = 0 then x else
                             foldli'(f, f(i - 1, sub(sl, i - 1), x), sl, i - 1)

  fun mapi f sl            = CharVector.fromList
                               (foldri (fn (i, a, l) => f(i, a)::l) [] sl)

  fun findi  f  sl         = findi'(f, sl, 0)
  and findi'(f, sl, i)     = if i = length sl then NONE else
                             if f(i, sub(sl, i)) then SOME(i, sub(sl, i)) else
                             findi'(f, sl, i + 1)

  fun app f sl             = appi (f o #2) sl
  fun map f sl             = mapi (f o #2) sl
  fun foldl f init sl      = foldli (fn (_, a, x) => f(a, x)) init sl
  fun foldr f init sl      = foldri (fn (_, a, x) => f(a, x)) init sl
  fun find f sl            = Option.map #2 (findi (f o #2) sl)
  fun exists f sl          = Option.isSome(find f sl)
  fun all f sl             = Bool.not(exists (Bool.not o f) sl)
end;
