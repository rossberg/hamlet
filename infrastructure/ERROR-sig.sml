(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Error handling.
 *)

signature ERROR =
sig
  (* Import *)

  type loc = Source.loc


  (* Export *)

  exception Error

  val print          : loc * string -> unit
  val warning        : loc * string -> unit
  val error          : loc * string -> 'a
  val errorLab       : loc * string * IdsCore.Lab -> 'a
  val errorVId       : loc * string * IdsCore.VId -> 'a
  val errorTyCon     : loc * string * IdsCore.TyCon -> 'a
  val errorTyVar     : loc * string * IdsCore.TyVar -> 'a
  val errorStrId     : loc * string * IdsCore.StrId -> 'a
  val errorSigId     : loc * string * IdsModule.SigId -> 'a
  val errorFunId     : loc * string * IdsModule.FunId -> 'a
  val errorLongVId   : loc * string * IdsCore.longVId -> 'a
  val errorLongTyCon : loc * string * IdsCore.longTyCon -> 'a
  val errorLongStrId : loc * string * IdsCore.longStrId -> 'a
end;
