(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML context for binding analysis
 *)

signature BINDING_CONTEXT =
sig
  (* Import *)

  type VId       = IdsCore.VId
  type TyCon     = IdsCore.TyCon
  type StrId     = IdsCore.StrId
  type longVId   = IdsCore.longVId
  type longTyCon = IdsCore.longTyCon
  type longStrId = IdsCore.longStrId

  type TyVarSet  = BindingObjectsCore.TyVarSet
  type TyStr     = BindingObjectsCore.TyStr
  type TyEnv     = BindingObjectsCore.TyEnv
  type ValStr    = BindingObjectsCore.ValStr
  type ValEnv    = BindingObjectsCore.ValEnv
  type Env       = BindingObjectsCore.Env
  type Context   = BindingObjectsCore.Context


  (* Operations *)

  val Uof           : Context -> TyVarSet
  val Eof           : Context -> Env

  val plusU         : Context * TyVarSet -> Context
  val plusE         : Context * Env      -> Context
  val plusVE        : Context * ValEnv   -> Context
  val plusTE        : Context * TyEnv    -> Context
  val plusVEandTE   : Context * (ValEnv * TyEnv) -> Context

  val findLongVId   : Context * longVId -> ValStr option
  val findLongTyCon : Context * longTyCon -> TyStr option
  val findLongStrId : Context * longStrId -> Env option
end;
