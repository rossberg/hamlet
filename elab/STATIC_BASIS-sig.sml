(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML static basis and environments of modules
 *
 * Definition, Section 5.1
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: Local modules
 *)

signature STATIC_BASIS =
sig
    (* Import *)

    type StrId		= IdsCore.StrId
    type SigId		= IdsModule.SigId
    type longTyCon	= IdsCore.longTyCon
    type longStrId	= IdsCore.longStrId
    type longSigId	= IdsModule.longSigId

    type Env		= StaticObjectsCore.Env
    type StrEnv		= StaticObjectsCore.StrEnv
    type SigEnv		= StaticObjectsCore.SigEnv
    type TyStr		= StaticObjectsCore.TyStr
    type Mod		= StaticObjectsCore.Mod
    type Context	= StaticObjectsCore.Context
    type TyVarSet	= StaticObjectsCore.TyVarSet
    type TyNameSet	= StaticObjectsCore.TyNameSet

    type Sig		= StaticObjectsModule.Sig
    type FunSig		= StaticObjectsModule.FunSig
    type Basis		= StaticObjectsModule.Basis


    (* Operations *)

    val empty :		Basis
    val fromTandE :	TyNameSet * Env   -> Basis
    val fromC :		Context           -> Basis

    val Tof :		Basis -> TyNameSet
    val Cof :		Basis -> Context

    val plus :		Basis * Basis     -> Basis
    val plusT :		Basis * TyNameSet -> Basis
    val oplusSE :	Basis * StrEnv    -> Basis
    val oplusG :	Basis * SigEnv    -> Basis
    val oplusE :	Basis * Env       -> Basis

    val findStrId :	Basis * StrId     -> Mod option
    val findSigId :	Basis * SigId     -> Sig option
    val findLongTyCon :	Basis * longTyCon -> TyStr option
    val findLongStrId :	Basis * longStrId -> Mod option
    val findLongSigId :	Basis * longSigId -> Sig option

    val tyvars :	Basis  -> TyVarSet
    val tynames :	Basis  -> TyNameSet
    val undetermined :	Basis  -> bool StampMap.map

    val toBindingBasis : Basis -> BindingBasis.Basis
end;
