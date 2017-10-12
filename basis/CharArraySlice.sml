(*
 * (c) Andreas Rossberg 2002-2013
 *
 * Standard ML Basis Library
 *)

structure CharArraySlice :> MONO_ARRAY_SLICE
  where type vector = CharVector.vector
  where type vector_slice = CharVectorSlice.slice
  where type array = CharArray.array
  where type elem = char =
struct
  open ArraySlice

  type elem         = Char.char
  type vector       = CharVector.vector
  type array        = elem array
  type slice        = elem slice
  type vector_slice = CharVectorSlice.slice

  fun vector sl = CharVector.tabulate(length sl, fn i => sub (sl, i))

  fun copyVec{src, dst, di} =
      if di < 0 orelse Array.length dst < di + CharVectorSlice.length src
      then raise Subscript
      else copyVec'(src, dst, di, 0)
  and copyVec'(src, dst, di, i) =
      if i = CharVectorSlice.length src then () else
      ( Array.update(dst, di + i, CharVectorSlice.sub(src, i));
        copyVec'(src, dst, di, i + 1) )
end;
