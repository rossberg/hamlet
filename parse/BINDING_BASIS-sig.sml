(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML environment for binding analysis
 *)

signature BINDING_BASIS =
sig
    (* Import *)

    type StrId		= IdsCore.StrId
    type SigId		= IdsModule.SigId
    type longTyCon	= IdsCore.longTyCon
    type longStrId	= IdsCore.longStrId
    type longSigId	= IdsModule.longSigId

    type Mod		= BindingObjectsCore.Mod
    type Env		= BindingObjectsCore.Env
    type ValEnv		= BindingObjectsCore.ValEnv
    type StrEnv		= BindingObjectsCore.StrEnv
    type SigEnv		= BindingObjectsCore.SigEnv
    type Context	= BindingObjectsCore.Context
    type Basis		= BindingObjectsModule.Basis


    (* Operations *)

    val empty :		Basis
    val fromE :		Env     -> Basis
    val fromG :		SigEnv  -> Basis
    val fromC :		Context -> Basis

    val Cof :		Basis   -> Context

    val plus :		Basis * Basis     -> Basis
    val plusE :		Basis * Env       -> Basis
    val plusSE :	Basis * StrEnv    -> Basis
    val plusG :		Basis * SigEnv    -> Basis

    val findStrId :	Basis * StrId     -> Mod option
    val findSigId :	Basis * SigId     -> Mod option
    val findLongTyCon :	Basis * longTyCon -> ValEnv option
    val findLongStrId :	Basis * longStrId -> Mod option
    val findLongSigId :	Basis * longSigId -> Mod option
end;
