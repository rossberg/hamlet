(*
 * (c) Andreas Rossberg 2002-2013
 *
 * Standard ML Basis Library
 *)

structure ArraySlice :> ARRAY_SLICE =
struct
  type 'a slice               = 'a Array.array * int * int

  fun base sl                 = sl : 'a slice
  fun length (a, i, n)        = n
  fun isEmpty sl              = length sl = 0
  fun sub((a, i, n), j)       = if j < 0 orelse n <= j then raise Subscript
                                else Array.sub(a, i+j)
  fun update((a, i, n), j, x) = if j < 0 orelse n <= j then raise Subscript
                                else Array.update(a, i+j, x)

  fun subslice(sl, i, NONE)   = subslice(sl, i, SOME(length sl - i))
    | subslice(sl, i, SOME n) = ( if i < 0 orelse n < 0 orelse length sl < i + n
                                  then raise Subscript
                                  else (#1 sl, #2 sl + i, n)
                                ) handle Overflow => raise Subscript
  fun full arr                = (arr, 0, Array.length arr)
  fun slice(arr, i, sz)       = subslice(full arr, i, sz)

  fun vector sl               = Vector.tabulate(length sl, fn i => sub (sl, i))

  fun getItem sl              = if isEmpty sl then NONE else
                                SOME(sub(sl, 0), subslice(sl, 1, NONE))

  fun appi  f  sl             = appi'(f, sl, 0)
  and appi'(f, sl, i)         = if i = length sl then () else
                                ( f(i, sub(sl, i)); appi'(f, sl, i + 1) )

  fun modifyi  f  sl          = modifyi'(f, sl, 0)
  and modifyi'(f, sl, i)      = if i = length sl then () else
                                ( update(sl, i, f(i, sub(sl, i)));
                                  modifyi'(f, sl, i + 1) )

  fun foldli f init sl        = foldli'(f, init, sl, 0)
  and foldli'(f, x, sl, i)    = if i = length sl then x else
                                foldli'(f, f(i, sub(sl, i), x), sl, i + 1)
  fun foldri f init sl        = foldri'(f, init, sl, length sl)
  and foldri'(f, x, sl, i)    = if i = 0 then x else
                                foldri'(f, f(i - 1, sub(sl, i-1), x), sl, i - 1)

  fun findi  f  sl            = findi'(f, sl, 0)
  and findi'(f, sl, i)        = if i = length sl then NONE
                                else if f(i, sub(sl, i))
                                then SOME(i, sub(sl, i))
                                else findi'(f, sl, i + 1)

  fun app f sl                = appi (f o #2) sl
  fun modify f sl             = modifyi (f o #2) sl
  fun foldl f init sl         = foldli (fn(_, a, x) => f(a, x)) init sl
  fun foldr f init sl         = foldri (fn(_, a, x) => f(a, x)) init sl
  fun find f sl               = Option.map #2 (findi (f o #2) sl)
  fun exists f sl             = Option.isSome(find f sl)
  fun all f sl                = Bool.not(exists (Bool.not o f) sl)

  fun collate f ((a1, i1, 0),  (a2, i2, 0))  = EQUAL
    | collate f ((a1, i1, 0),  (a2, i2, n2)) = LESS
    | collate f ((a1, i1, n1), (a2, i2, 0) ) = GREATER
    | collate f ((a1, i1, n1), (a2, i2, n2)) =
      case f(Array.sub(a1, i1), Array.sub(a2, i2)) of
        EQUAL => collate f ((a1, i1 + 1, n1 - 1), (a2, i2 + 1, n2 - 1))
      | other => other

  fun copy{src, dst, di} =
      if di < 0 orelse Array.length dst < di + length src
      then raise Subscript
      else if #2 src > di
      then copy'(src, dst, di, 0, length src, 1)
      else copy'(src, dst, di, length src - 1, ~1, ~1)
  and copy'(src, dst, di, i, to, by) =
      if i = to then () else
      ( Array.update(dst, di + i, sub(src, i));
        copy'(src, dst, di, i + by, to, by) )

  fun copyVec{src, dst, di} =
      if di < 0 orelse Array.length dst < di + VectorSlice.length src
      then raise Subscript
      else copyVec'(src, dst, di, 0)
  and copyVec'(src, dst, di, i) =
      if i = VectorSlice.length src then () else
      ( Array.update(dst, di + i, VectorSlice.sub(src, i));
        copyVec'(src, dst, di, i + 1) )
end;
