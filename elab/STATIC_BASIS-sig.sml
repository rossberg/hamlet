(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML static basis and environments of modules
 *
 * Definition, Section 5.1
 *)

signature STATIC_BASIS =
sig
  (* Import *)

  type StrId     = IdsCore.StrId
  type SigId     = IdsModule.SigId
  type FunId     = IdsModule.FunId
  type longStrId = IdsCore.longStrId
  type longTyCon = IdsCore.longTyCon

  type Env       = StaticObjectsCore.Env
  type StrEnv    = StaticObjectsCore.StrEnv
  type TyStr     = StaticObjectsCore.TyStr
  type Context   = StaticObjectsCore.Context
  type TyVarSet  = StaticObjectsCore.TyVarSet
  type TyNameSet = StaticObjectsCore.TyNameSet

  type Sig       = StaticObjectsModule.Sig
  type FunSig    = StaticObjectsModule.FunSig
  type SigEnv    = StaticObjectsModule.SigEnv
  type FunEnv    = StaticObjectsModule.FunEnv
  type Basis     = StaticObjectsModule.Basis


  (* Operations *)

  val empty          : Basis
  val fromTandE      : TyNameSet * Env    -> Basis
  val fromTandF      : TyNameSet * FunEnv -> Basis
  val fromTandG      : TyNameSet * SigEnv -> Basis

  val Tof            : Basis -> TyNameSet
  val Cof            : Basis -> Context

  val plus           : Basis * Basis     -> Basis
  val plusT          : Basis * TyNameSet -> Basis
  val oplusSE        : Basis * StrEnv    -> Basis
  val oplusG         : Basis * SigEnv    -> Basis
  val oplusF         : Basis * FunEnv    -> Basis
  val oplusE         : Basis * Env       -> Basis

  val findStrId      : Basis * StrId     -> Env option
  val findSigId      : Basis * SigId     -> Sig option
  val findFunId      : Basis * FunId     -> FunSig option
  val findLongStrId  : Basis * longStrId -> Env option
  val findLongTyCon  : Basis * longTyCon -> TyStr option

  val tyvars         : Basis  -> TyVarSet
  val tynames        : Basis  -> TyNameSet
  val tynamesF       : FunEnv -> TyNameSet
  val tynamesG       : SigEnv -> TyNameSet
  val undetermined   : Basis  -> bool StampMap.map

  val disjoint       : Basis * Basis -> bool

  val toBindingBasis : Basis -> BindingBasis.Basis
end;
