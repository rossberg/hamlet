(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note: Kept transparent to allow easy implementation of Word8VectorSlice.
 *)

structure Word8Vector : MONO_VECTOR
  where type elem = Word8.word =
struct
  open Vector

  type elem   = Word8.word
  type vector = elem vector
end;
