(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note:
 * - Array type kept transparent to allow trivial implementation
 *   of CharArraySlice.
 *)

structure CharArray : MONO_ARRAY
  where type vector = CharVector.vector
  where type elem = char =
struct
  open Array

  type array  = char array
  type elem   = char
  type vector = CharVector.vector

  fun vector arr = CharVector.tabulate(length arr, fn i => sub(arr, i))

  fun copyVec{src, dst, di} =
      if di < 0 orelse length dst < di + CharVector.length src
      then raise Subscript
      else copyVec'(src, dst, di, 0)
  and copyVec'(src, dst, di, i) =
      if i = CharVector.length src then () else
      ( update(dst, di + i, CharVector.sub(src, i));
        copyVec'(src, dst, di, i + 1) )
end;
