(*
 * (c) Andreas Rossberg 2002-2013
 *
 * Standard ML Basis Library
 *)

structure VectorSlice :> VECTOR_SLICE =
struct
  type 'a slice         = 'a Vector.vector * int * int

  fun base sl           = sl : 'a slice
  fun length(v, i, n)   = n
  fun isEmpty sl        = length sl = 0
  fun sub((v, i, n), j) = if j < 0 orelse n <= j then raise Subscript else
                          Vector.sub(v, i + j)

  fun subslice(sl, i, NONE)   = subslice(sl, i, SOME(length sl - i))
    | subslice(sl, i, SOME n) =
      ( if i < 0 orelse n < 0 orelse length sl < i+n
        then raise Subscript
        else (#1 sl, #2 sl + i, n)
      ) handle Overflow => raise Subscript

  fun full v               = (v, 0, Vector.length v)
  fun slice(v, i, sz)      = subslice(full v, i, sz)

  fun vector sl            = Vector.tabulate(length sl, fn i => sub (sl, i))
  fun concat l             = Vector.concat(List.map vector l)

  fun getItem sl           = if isEmpty sl then NONE else
                             SOME(sub(sl, 0), subslice(sl, 1, NONE))

  fun appi  f  sl          = appi'(f, sl, 0)
  and appi'(f, sl, i)      = if i = length sl then () else
                             ( f(i, sub(sl, i)); appi'(f, sl, i + 1) )

  fun foldli f init sl     = foldli'(f, init, sl, 0)
  and foldli'(f, x, sl, i) = if i = length sl then x else
                             foldli'(f, f(i, sub(sl, i), x), sl, i + 1)
  fun foldri f init sl     = foldri'(f, init, sl, length sl)
  and foldri'(f, x, sl, i) = if i = 0 then x else
                             foldli'(f, f(i - 1, sub(sl, i - 1), x), sl, i - 1)

  fun mapi f sl            = let fun ff(i, a, l) = f(i, a)::l
                             in Vector.fromList(List.rev (foldli ff [] sl)) end

  fun findi  f  sl          = findi'(f, sl, 0)
  and findi'(f, sl, i)      = if i = length sl then NONE
                              else if f(i, sub(sl, i))
                              then SOME(i, sub(sl, i))
                              else findi'(f, sl, i + 1)

  fun app f sl              = appi (f o #2) sl
  fun map f sl              = mapi (f o #2) sl
  fun foldl f init sl       = foldli (fn (_, a, x) => f(a, x)) init sl
  fun foldr f init sl       = foldri (fn (_, a, x) => f(a, x)) init sl
  fun find f sl             = Option.map #2 (findi (f o #2) sl)
  fun exists f sl           = Option.isSome(find f sl)
  fun all f sl              = Bool.not(exists (Bool.not o f) sl)

  fun collate f ((v1, i1, 0),  (v2, i2, 0))  = EQUAL
    | collate f ((v1, i1, 0),  (v2, i2, n2)) = LESS
    | collate f ((v1, i1, n1), (v2, i2, 0) ) = GREATER
    | collate f ((v1, i1, n1), (v2, i2, n2)) =
      case f(Vector.sub(v1, i1), Vector.sub(v2, i2)) of
        EQUAL => collate f ((v1, i1 + 1, n1 - 1), (v2, i2 + 1, n2 - 1))
      | other => other
end;
