(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure Vector :> VECTOR =
struct
  type 'a vector = 'a vector

  val maxLen     = use{b = "Vector.maxLen"} () : int
  fun length x   = (use{b = "Vector.length"} : 'a vector -> int) x
  fun sub x      = (use{b = "Vector.sub"} : 'a vector * int -> 'a) x
  fun fromList x = (use{b = "Vector.fromList"} : 'a list-> 'a vector) x

  fun tabulate(n, f)        = fromList(List.tabulate(n, f))

  fun update(vec, i, x)     = tabulate(length vec,
                                fn i' => if i = i' then x else sub(vec, i'))

  fun foldli f init vec     = foldli'(f, init, vec, 0)
  and foldli'(f, x, vec, i) = if i = length vec then x else
                              foldli'(f, f(i, sub(vec, i), x), vec, i + 1)
  fun foldri f init vec     = foldri'(f, init, vec, length vec)
  and foldri'(f, x, vec, i) = if i = 0 then x else
                              foldri'(f, f(i - 1, sub(vec, i-1), x), vec, i - 1)

  fun foldl f init vec      = foldli (fn (_, a, x) => f(a, x)) init vec
  fun foldr f init vec      = foldri (fn (_, a, x) => f(a, x)) init vec

  fun appi f vec            = List.app f
                                (foldri (fn(i, a, l) => (i, a)::l) [] vec)
  fun app f vec             = List.app f (foldr (fn(a, l) => a::l) [] vec)

  fun mapi f vec            = fromList(List.map f
                                (foldri (fn(i, a, l) => (i, a)::l) [] vec))
  fun map f vec             = fromList(List.map f
                                (foldr (fn(a, l) => a::l) [] vec))

  fun findi  f  vec         = findi'(f, vec, 0)
  and findi'(f, vec, i)     = if i = length vec then NONE
                              else if f(i, sub(vec, i))
                              then SOME(i, sub(vec, i))
                              else findi'(f, vec, i + 1)

  fun find f vec            = Option.map #2 (findi (f o #2) vec)
  fun exists f vec          = Option.isSome(find f vec)
  fun all f vec             = Bool.not(exists (Bool.not o f) vec)

  fun collate f (vec1, vec2) = collate'(f, vec1, vec2, 0)
  and collate'(f, vec1, vec2, i) =
      case (i = length vec1, i = length vec2) of
        (true,  true)  => EQUAL
      | (true,  false) => LESS
      | (false, true)  => GREATER
      | (false, false) =>
        case f(sub(vec1, i), sub(vec2, i)) of
          EQUAL => collate'(f, vec1, vec2, i + 1)
        | other => other

  fun concat l = fromList(List.concat(List.map (foldr op:: []) l))
end

type 'a vector = 'a Vector.vector;
