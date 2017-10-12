(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML dynamic basis and environments of modules
 *
 * Definition, Section 7.2
 *)

signature DYNAMIC_BASIS =
sig
  (* Import *)

  type StrId          = IdsCore.StrId
  type SigId          = IdsModule.SigId
  type FunId          = IdsModule.FunId
  type longStrId      = IdsCore.longStrId
  type longTyCon      = IdsCore.longTyCon

  type Env            = DynamicObjectsCore.Env
  type ValEnv         = DynamicObjectsCore.ValEnv
  type StrEnv         = DynamicObjectsCore.StrEnv
  type SigEnv         = DynamicObjectsModule.SigEnv
  type FunEnv         = DynamicObjectsModule.FunEnv
  type Int            = DynamicObjectsModule.Int
  type FunctorClosure = DynamicObjectsModule.FunctorClosure
  type Basis          = DynamicObjectsModule.Basis


  (* Operations *)

  val empty         : Basis
  val fromE         : Env -> Basis
  val fromF         : FunEnv -> Basis
  val fromG         : SigEnv -> Basis

  val Eof           : Basis -> Env

  val plus          : Basis * Basis -> Basis
  val plusSE        : Basis * StrEnv -> Basis
  val plusG         : Basis * SigEnv -> Basis
  val plusF         : Basis * FunEnv -> Basis
  val plusE         : Basis * Env -> Basis

  val findStrId     : Basis * StrId -> Env option
  val findSigId     : Basis * SigId -> Int option
  val findFunId     : Basis * FunId -> FunctorClosure option
  val findLongStrId : Basis * longStrId -> Env option
  val findLongTyCon : Basis * longTyCon -> ValEnv option
end;
