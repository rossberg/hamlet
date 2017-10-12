(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of identifiers into JavaScript.
 *)

signature JS_TRANSLATE_ID =
sig
  (* Import *)

  type Lab       = SyntaxCore.Lab
  type VId       = SyntaxCore.VId
  type StrId     = SyntaxCore.StrId
  type FunId     = SyntaxModule.FunId
  type longVId   = SyntaxCore.longVId
  type longStrId = SyntaxCore.longStrId

  type name      = JSSyntax.name
  type var       = JSSyntax.var
  type expr      = JSSyntax.expr


  (* Export *)

  val translateLab'      : Lab.Lab -> name
  val translateVId'      : VId.Id -> var
  val translateStrId'    : StrId.Id -> var
  val translateFunId'    : FunId.Id -> var

  val translateLab       : Lab -> name
  val translateVId       : VId -> var
  val translateStrId     : StrId -> var
  val translateFunId     : FunId -> var

  val translateLongVId   : longVId -> expr
  val translateLongStrId : longStrId -> expr
end;
