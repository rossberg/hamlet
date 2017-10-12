(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library value representation
 *
 * Definition, Sections 6.2 and E.1
 * Standard Basis Specification
 *
 * Note:
 * - We have to separate this from the rest of the Library module in order
 *   to avoid cycles in our architecture.
 *)

structure LibrarySVal : LIBRARY_SVAL =
struct
    datatype IntSVal                        = INT of int
    datatype WordSVal                       = WORD of word | WORD8 of Word8.word
    datatype RealSVal                       = REAL of real
    datatype CharSVal                       = CHAR of char
    datatype StringSVal                     = STRING of string

    val wordToWord8                         = Word8.fromLarge o Word.toLarge
    val word8ToWord                         = Word.fromLarge o Word8.toLarge
    val word8ToWordX                        = Word.fromLarge o Word8.toLargeX

    fun intToString(INT i)                  = Int.toString i
    fun wordToString(WORD w)                = Word.toString w
      | wordToString(WORD8 w)               = Word8.toString w
    fun realToString(REAL r)                = Real.toString r
    fun charToString(CHAR c)                = Char.toString c
    fun stringToString(STRING s)            = String.toString s

    fun compareInt(INT i1, INT i2)          = Int.compare(i1, i2)
    fun compareWord(WORD w1, WORD w2)       = Word.compare(w1, w2)
      | compareWord(WORD w1, WORD8 w2)      = Word.compare(w1, word8ToWord w2)
      | compareWord(WORD8 w1, WORD w2)      = Word.compare(word8ToWord w1, w2)
      | compareWord(WORD8 w1, WORD8 w2)     = Word8.compare(w1, w2)
    fun compareReal(REAL r1, REAL r2)       = Real.compare(r1, r2)
    fun compareChar(CHAR c1, CHAR c2)       = Char.compare(c1, c2)
    fun compareString(STRING s1, STRING s2) = String.compare(s1, s2)
end;
