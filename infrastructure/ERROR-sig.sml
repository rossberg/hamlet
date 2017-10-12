(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Error handling.
 *)

signature ERROR =
sig
    (* Import *)

    type position		= Source.info


    (* Export *)

    exception Error

    val warning :		position * string -> unit
    val error :			position * string -> 'a
    val errorLab :		position * string * IdsCore.Lab -> 'a
    val errorVId :		position * string * IdsCore.VId -> 'a
    val errorTyCon :		position * string * IdsCore.TyCon -> 'a
    val errorTyVar :		position * string * IdsCore.TyVar -> 'a
    val errorStrId :		position * string * IdsCore.StrId -> 'a
    val errorSigId :		position * string * IdsModule.SigId -> 'a
    val errorLongVId :		position * string * IdsCore.longVId -> 'a
    val errorLongTyCon :	position * string * IdsCore.longTyCon -> 'a
    val errorLongStrId :	position * string * IdsCore.longStrId -> 'a
    val errorLongSigId :	position * string * IdsModule.longSigId -> 'a
end;
