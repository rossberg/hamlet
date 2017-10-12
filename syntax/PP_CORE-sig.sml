(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Printer for abstract core syntax
 *)

signature PP_CORE =
sig
  type VId        = SyntaxCore.VId
  type TyCon      = SyntaxCore.TyCon
  type StrId      = SyntaxCore.StrId
  type longVId    = SyntaxCore.longVId
  type longTyCon  = SyntaxCore.longTyCon
  type longStrId  = SyntaxCore.longStrId
  type TyVar      = SyntaxCore.TyVar
  type Ty         = SyntaxCore.Ty
  type Dec        = SyntaxCore.Dec
  type 'a seq     = 'a SyntaxCore.seq

  val ppVId       : TextIO.outstream * int * VId -> unit
  val ppTyCon     : TextIO.outstream * int * TyCon -> unit
  val ppStrId     : TextIO.outstream * int * StrId -> unit
  val ppLongVId   : TextIO.outstream * int * longVId -> unit
  val ppLongTyCon : TextIO.outstream * int * longTyCon -> unit
  val ppLongStrId : TextIO.outstream * int * longStrId -> unit
  val ppTyVarseq  : TextIO.outstream * int * TyVar seq -> unit

  val ppTy        : TextIO.outstream * int * Ty -> unit
  val ppDec       : TextIO.outstream * int * Dec -> unit
end;
