(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note:
 * - We must keep the array type transparent in order to keep its
 *   special equality property.
 *)

structure Array : ARRAY =
struct
  type 'a array               = 'a ref vector ref
  type 'a vector              = 'a vector

  val maxLen                  = Vector.maxLen

  fun array(n, init)          = ref(Vector.tabulate(n, fn _ => ref init))
  fun fromList l              = ref(Vector.fromList(List.map ref l))
  fun tabulate(n, f)          = ref(Vector.tabulate(n, ref o f))

  fun length arr              = Vector.length(!arr)
  fun sub(arr, i)             = !(Vector.sub(!arr, i))
  fun update(arr, i, x)       = Vector.sub(!arr, i) := x
  fun vector arr              = Vector.map ! (!arr)

  fun deref2(i, r)            = (i, !r)
  fun deref3(i, r, x)         = (i, !r, x)

  fun appi f arr              = Vector.appi (f o deref2) (!arr)
  fun modifyi f arr           = Vector.appi (fn (i, r) => r := f(i, !r)) (!arr)
  fun foldli f init arr       = Vector.foldli (f o deref3) init (!arr)
  fun foldri f init arr       = Vector.foldri (f o deref3) init (!arr)
  fun findi f arr             = Option.map deref2
                                  (Vector.findi (f o deref2) (!arr))

  fun app f                   = appi (f o #2)
  fun modify f                = modifyi (f o #2)
  fun foldl f                 = foldli (fn (_, a, x) => f(a, x))
  fun foldr f                 = foldri (fn (_, a, x) => f(a, x))
  fun find f arr              = Option.map #2 (findi (f o #2) arr)
  fun exists f arr            = Option.isSome(find f arr)
  fun all f arr               = Bool.not(exists (Bool.not o f) arr)

  fun collate  f (a1, a2)     = collate'(f, a1, a2, 0)
  and collate'(f, a1, a2, i)  =
      case (i = length a1, i = length a2) of
        (true,  true ) => EQUAL
      | (true,  false) => LESS
      | (false, true ) => GREATER
      | (false, false) =>
      case f(sub(a1, i), sub(a2, i)) of
        EQUAL => collate'(f, a1, a2, i+1)
      | other => other

  fun copy{src, dst, di} =
      if di < 0 orelse length dst < di + length src
      then raise Subscript
      else copy'(src, dst, di, 0)
  and copy'(src, dst, di, i) =
      if i = length src then () else
      ( update(dst, di + i, sub(src, i));
        copy'(src, dst, di, i + 1) )

  fun copyVec{src, dst, di} =
      if di < 0 orelse length dst < di + Vector.length src
      then raise Subscript
      else copyVec'(src, dst, di, 0)
  and copyVec'(src, dst, di, i) =
      if i = Vector.length src then () else
      ( update(dst, di + i, Vector.sub(src, i));
        copyVec'(src, dst, di, i + 1) )
end

type 'a array = 'a Array.array;
