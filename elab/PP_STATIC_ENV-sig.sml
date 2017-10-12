(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the static environment
 *)

signature PP_STATIC_ENV =
sig
  type ValEnv     = StaticObjectsCore.ValEnv
  type TyEnv      = StaticObjectsCore.TyEnv
  type Env        = StaticObjectsCore.Env
  type TyNameSet  = StaticObjectsCore.TyNameSet

  val ppEnv       : Env -> PrettyPrint.doc
  val ppSig       : TyNameSet * Env -> PrettyPrint.doc
  val ppTyNameSet : TyNameSet -> PrettyPrint.doc

  val ppTyEnv     : TyNameSet * TyEnv -> PrettyPrint.doc
  val ppExEnv     : ValEnv -> PrettyPrint.doc
end;
