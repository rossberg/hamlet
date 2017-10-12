(*
 * (c) Andreas Rossberg 2002-2013
 *
 * Standard ML Basis Library
 *)

structure Word8ArraySlice :> MONO_ARRAY_SLICE
  where type vector       = Word8Vector.vector
  where type vector_slice = Word8VectorSlice.slice
  where type array        = Word8Array.array
  where type elem         = Word8.word =
struct
  open ArraySlice

  type elem         = Word8.word
  type vector       = Word8Vector.vector
  type array        = elem array
  type slice        = elem slice
  type vector_slice = Word8VectorSlice.slice

  fun copyVec{src, dst, di} =
      if di < 0 orelse Array.length dst < di + Word8VectorSlice.length src
      then raise Subscript
      else copyVec'(src, dst, di, 0)
  and copyVec'(src, dst, di, i) =
      if i = Word8VectorSlice.length src then () else
      ( Array.update(dst, di + i, Word8VectorSlice.sub(src, i));
        copyVec'(src, dst, di, i + 1) )
end;
