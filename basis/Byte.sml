(*
 * (c) Andreas Rossberg 2002-2013
 *
 * Standard ML Basis Library
 *)

structure Byte :> BYTE =
struct
  fun byteToChar i = Char.chr(Word8.toInt i)
  fun charToByte c = Word8.fromInt(Char.ord c)

  fun stringToBytes s =
      Word8Vector.tabulate(CharVector.length s,
        fn i => charToByte(CharVector.sub(s, i)))

  fun bytesToString v =
      CharVector.tabulate(Word8Vector.length v,
        fn i => byteToChar(Word8Vector.sub(v, i)))

  fun unpackStringVec sl =
      CharVector.tabulate(Word8VectorSlice.length sl,
         fn i => byteToChar(Word8VectorSlice.sub(sl, i)))

  fun unpackString sl =
      CharVector.tabulate(Word8ArraySlice.length sl,
        fn i => byteToChar(Word8ArraySlice.sub(sl, i)))

  fun packString(arr, i, ss) =
      Word8Array.copyVec{
        src = stringToBytes(Substring.string ss), dst = arr, di = i}
end;
