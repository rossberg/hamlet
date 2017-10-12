(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the static environment
 *)

signature PP_STATIC_ENV =
sig
    type ValEnv		= StaticObjectsCore.ValEnv
    type TyEnv		= StaticObjectsCore.TyEnv
    type StrEnv		= StaticObjectsCore.StrEnv
    type SigEnv		= StaticObjectsCore.SigEnv
    type Env		= StaticObjectsCore.Env
    type Mod		= StaticObjectsCore.Mod
    type TyNameSet	= StaticObjectsCore.TyNameSet

    val ppEnv :		Env -> PrettyPrint.doc
    val ppTyNameSet :	TyNameSet -> PrettyPrint.doc
    val ppSig :		TyNameSet * Mod -> PrettyPrint.doc

    val ppSigEnv :	TyNameSet * SigEnv -> PrettyPrint.doc
    val ppTyEnv :	TyNameSet * TyEnv -> PrettyPrint.doc
    val ppExEnv :	ValEnv -> PrettyPrint.doc
end;
