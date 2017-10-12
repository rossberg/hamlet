(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note:
 *   Array type kept transparent to allow trivial implementation of
 *   Word8ArraySlice.
 *)

structure Word8Array : MONO_ARRAY
  where type vector = Word8Vector.vector
  where type elem = Word8.word =
struct
  open Array

  type elem   = Word8.word
  type vector = Word8Vector.vector
  type array  = elem array
end;
