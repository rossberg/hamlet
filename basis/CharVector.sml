(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure CharVector :> MONO_VECTOR
  where type vector = String.string
  where type elem = Char.char =
struct
  type vector = String.string
  type elem   = Char.char

  val maxLen  = String.maxSize
  val length  = String.size
  val sub     = String.sub
  val concat  = String.concat
  val collate = String.collate

  val fromList = use{b = "CharVector.fromList"} : elem list-> vector
  fun tabulate(n, f) = fromList(List.tabulate(n, f))
  fun update(vec, i, x) =
      tabulate(length vec, fn j => if j = i then x else sub(vec, i))

  fun appi  f  vec          = appi'(f, vec, 0)
  and appi'(f, vec, i)      = if i = length vec then () else
                              ( f(i, sub(vec, i)); appi'(f, vec, i + 1) )

  fun foldli f init vec     = foldli'(f, init, vec, 0)
  and foldli'(f, x, vec, i) = if i = length vec then x else
                              foldli'(f, f(i, sub(vec, i), x), vec, i + 1)
  fun foldri f init vec     = foldri'(f, init, vec, length vec)
  and foldri'(f, x, vec, i) = if i = 0 then x else
                              foldli'(f, f(i - 1, sub(vec, i-1), x), vec, i - 1)

  fun mapi f vec            = fromList
                                (foldri (fn (i, x, l) => f(i, x)::l) [] vec)

  fun findi  f  vec         = findi'(f, vec, 0)
  and findi'(f, vec, i)     = if i = length vec then NONE else
                              if f(i, sub(vec, i)) then SOME(i, sub(vec, i))
                              else findi'(f, vec, i + 1)

  fun app f vec             = appi (f o #2) vec
  fun map f vec             = mapi (f o #2) vec
  fun foldl f init vec      = foldli (fn (_, a, x) => f(a, x)) init vec
  fun foldr f init vec      = foldri (fn (_, a, x) => f(a, x)) init vec
  fun find f vec            = Option.map #2 (findi (f o #2) vec)
  fun exists f vec          = Option.isSome(find f vec)
  fun all f vec             = Bool.not(exists (Bool.not o f) vec)
end;
