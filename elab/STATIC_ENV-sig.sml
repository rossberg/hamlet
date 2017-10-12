(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environments of the static semantics of the core
 *
 * Definition, Sections 4.2, 4.3, 4.8, 4.9, and 5.5
 *
 * Notes: see StaticObjectsCore.sml
 *)

signature STATIC_ENV =
sig
  (* Inheritance *)

  include GENERIC_ENV
    where type Env    = StaticObjectsCore.Env
      and type ValStr = StaticObjectsCore.ValStr
      and type TyStr  = StaticObjectsCore.TyStr


  (* Import *)

  type TyNameSet   = StaticObjectsCore.TyNameSet
  type TyVarSet    = StaticObjectsCore.TyVarSet
  type Realisation = Type.Realisation


  (* Operations *)

  val tyvars       : Env -> TyVarSet
  val tyvarsVE     : ValEnv -> TyVarSet
  val tynames      : Env -> TyNameSet
  val tynamesVE    : ValEnv -> TyNameSet
  val tynamesTE    : TyEnv  -> TyNameSet
  val tynamesSE    : StrEnv -> TyNameSet
  val undetermined : Env -> bool StampMap.map

  val isWellFormed : Env -> bool

  val Clos         : ValEnv -> ValEnv
  val Abs          : TyEnv * Env -> Env
  val realise      : Realisation -> Env -> Env

  val enriches     : Env * Env -> bool
  val equalsVE     : ValEnv * ValEnv -> bool

  val maximiseEquality : TyEnv -> unit
end;
