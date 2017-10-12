(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environment for binding analysis
 *)

signature BINDING_BASIS =
sig
  (* Import *)

  type StrId     = IdsCore.StrId
  type SigId     = IdsModule.SigId
  type FunId     = IdsModule.FunId
  type longStrId = IdsCore.longStrId
  type longTyCon = IdsCore.longTyCon

  type Env       = BindingObjectsCore.Env
  type ValEnv    = BindingObjectsCore.ValEnv
  type StrEnv    = BindingObjectsCore.StrEnv
  type Context   = BindingObjectsCore.Context
  type SigEnv    = BindingObjectsModule.SigEnv
  type FunEnv    = BindingObjectsModule.FunEnv
  type Basis     = BindingObjectsModule.Basis


  (* Operations *)

  val empty         : Basis
  val fromE         : Env -> Basis
  val fromF         : FunEnv -> Basis
  val fromG         : SigEnv -> Basis

  val Cof           : Basis -> Context

  val plus          : Basis * Basis -> Basis
  val plusSE        : Basis * StrEnv -> Basis
  val plusG         : Basis * SigEnv -> Basis
  val plusF         : Basis * FunEnv -> Basis
  val plusE         : Basis * Env -> Basis

  val findStrId     : Basis * StrId -> Env option
  val findSigId     : Basis * SigId -> Env option
  val findFunId     : Basis * FunId -> Env option
  val findLongStrId : Basis * longStrId -> Env option
  val findLongTyCon : Basis * longTyCon -> ValEnv option
end;
