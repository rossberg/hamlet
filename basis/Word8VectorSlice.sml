(*
 * (c) Andreas Rossberg 2002-2013
 *
 * Standard ML Basis Library
 *)

structure Word8VectorSlice :> MONO_VECTOR_SLICE
  where type vector = Word8Vector.vector
  where type elem   = Word8.word =
struct
  open VectorSlice

  type elem   = Word8.word
  type vector = Word8Vector.vector
  type slice  = elem slice
end;
