(*
 * (c) Andreas Rossberg 2002-2007
 *
 * Standard ML Basis Library
 *)

structure Byte :> BYTE =
struct
    structure C		= Char
    structure W		= Word8
    structure CV	= CharVector
    structure WV	= Word8Vector
    structure WA	= Word8Array
    structure WVS	= Word8VectorSlice
    structure WAS	= Word8ArraySlice

    fun byteToChar i	= Char.chr(Word8.toInt i)
    fun charToByte c	= Word8.fromInt(Char.ord c)

    fun stringToBytes s	= WV.tabulate(CV.length s,
				      fn i => charToByte(CV.sub(s, i)))
    fun bytesToString v	= CV.tabulate(WV.length v,
				      fn i => byteToChar(WV.sub(v, i)))
    fun unpackStringVec sl = CV.tabulate(WVS.length sl,
				      fn i => byteToChar(WVS.sub(sl, i)))
    fun unpackString sl	= CV.tabulate(WAS.length sl,
				      fn i => byteToChar(WAS.sub(sl, i)))

    fun packString(arr, i, ss) =
	WA.copyVec{src=stringToBytes(Substring.string ss), dst=arr, di=i}
end;
