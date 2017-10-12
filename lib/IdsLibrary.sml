(*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML Basis Library identifiers
 *)

structure IdsLibrary =
struct
  (* Structure, type, exception and value identifiers *)

  val e = IdStatus.e
  val v = IdStatus.v

  val stridIO       = StrId.fromString "IO"
  val stridWord8    = StrId.fromString "Word8"
  val tyconWord8    = TyCon.fromString "word"
  val tyconVector   = TyCon.fromString "vector"

  val vid_Chr       = VId.fromString "Chr"
  val vid_Div       = VId.fromString "Div"
  val vid_Domain    = VId.fromString "Domain"
  val vid_Overflow  = VId.fromString "Overflow"
  val vid_Size      = VId.fromString "Size"
  val vid_Subscript = VId.fromString "Subscript"
  val vid_Io        = VId.fromString "Io"
  val vid_SysErr    = VId.fromString "SysErr"

  val vidAbs        = VId.fromString "abs"
  val vidNeg        = VId.fromString "~"
  val vidPlus       = VId.fromString "+"
  val vidMinus      = VId.fromString "-"
  val vidTimes      = VId.fromString "*"
  val vidDiv        = VId.fromString "div"
  val vidMod        = VId.fromString "mod"
  val vidBy         = VId.fromString "/"
  val vidLess       = VId.fromString "<"
  val vidGreater    = VId.fromString ">"
  val vidLessEq     = VId.fromString "<="
  val vidGreaterEq  = VId.fromString ">="

  val vidUse        = VId.fromString "use"
end;
