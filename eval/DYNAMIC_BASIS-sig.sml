(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML dynamic basis and environments of modules
 *
 * Definition, Section 7.2
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

signature DYNAMIC_BASIS =
sig
    (* Import *)

    type StrId		= IdsCore.StrId
    type SigId		= IdsModule.SigId
    type longTyCon	= IdsCore.longTyCon
    type longStrId	= IdsCore.longStrId
    type longSigId	= IdsModule.longSigId

    type Env		= DynamicObjectsCore.Env
    type ValEnv		= DynamicObjectsCore.ValEnv
    type StrEnv		= DynamicObjectsCore.StrEnv
    type SigEnv		= DynamicObjectsModule.SigEnv
    type Mod		= DynamicObjectsCore.Mod
    type Int		= DynamicObjectsModule.Int
    type Basis		= DynamicObjectsModule.Basis


    (* Operations *)

    val empty :		Basis
    val fromE :		Env    -> Basis

    val Eof :		Basis -> Env

    val plus :		Basis * Basis     -> Basis
    val plusE :		Basis * Env       -> Basis
    val plusSE :	Basis * StrEnv    -> Basis
    val plusG :		Basis * SigEnv    -> Basis

    val findStrId :	Basis * StrId     -> Mod option
    val findSigId :	Basis * SigId     -> Int option
    val findLongTyCon :	Basis * longTyCon -> ValEnv option
    val findLongStrId :	Basis * longStrId -> Mod option
    val findLongSigId :	Basis * longSigId -> Int option
end;
