(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML contexts
 *
 * Definition, Sections 4.2, 4.3, 4.7, and 4.9
 * + RFC: Higher-order functors
 * + RFC: Local modules
 * + RFC: First-class modules
 *)

signature CONTEXT =
sig
    (* Import *)

    type VId		= IdsCore.VId
    type TyCon		= IdsCore.TyCon
    type StrId		= IdsCore.StrId
    type longVId	= IdsCore.longVId
    type longTyCon	= IdsCore.longTyCon
    type longStrId	= IdsCore.longStrId
    type longSigId	= IdsModule.longSigId

    type TyNameSet	= StaticObjectsCore.TyNameSet
    type TyVarSet	= StaticObjectsCore.TyVarSet
    type Mod		= StaticObjectsCore.Mod
    type StrEnv		= StaticObjectsCore.StrEnv
    type TyStr		= StaticObjectsCore.TyStr
    type TyEnv		= StaticObjectsCore.TyEnv
    type ValStr		= StaticObjectsCore.ValStr
    type ValEnv		= StaticObjectsCore.ValEnv
    type Env		= StaticObjectsCore.Env
    type Context	= StaticObjectsCore.Context
    type Sig'		= StaticObjectsCore.Sig'


    (* Operations *)

    val Tof :		Context -> TyNameSet
    val Uof :		Context -> TyVarSet
    val Eof :		Context -> Env

    val plusT :		Context * TyNameSet -> Context
    val plusU :		Context * TyVarSet  -> Context
    val plusVE :	Context * ValEnv    -> Context
    val oplusE :	Context * Env       -> Context
    val oplusTE :	Context * TyEnv     -> Context
    val oplusSE :	Context * StrEnv    -> Context
    val oplusVEandTE :	Context * (ValEnv * TyEnv) -> Context

    val findVId :	Context * VId       -> ValStr option
    val findTyCon :	Context * TyCon     -> TyStr option
    val findStrId :	Context * StrId     -> Mod option
    val findLongVId :	Context * longVId   -> ValStr option
    val findLongTyCon :	Context * longTyCon -> TyStr option
    val findLongStrId :	Context * longStrId -> Mod option
    val findLongSigId :	Context * longSigId -> Sig' option

    val tyvars :	Context -> TyVarSet
    val tynames :	Context -> TyNameSet
    val undetermined :	Context -> bool StampMap.map
end;
