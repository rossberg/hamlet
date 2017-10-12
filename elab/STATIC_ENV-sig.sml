(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML environments of the static semantics of the core
 *
 * Definition, Sections 4.2, 4.3, 4.8, 4.9, and 5.5
 * + RFC: Abstype as derived
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *
 * Note:
 *     We call the domain type of value environments ValStr.
 *)

signature STATIC_ENV =
sig
    (* Inheritance *)

    include GENERIC_ENV
    where type Env		= StaticObjectsCore.Env
(**)where type ValStr		= StaticObjectsCore.ValStr
(**)where type TyStr		= StaticObjectsCore.TyStr
(**)where type ModStr		= StaticObjectsCore.Mod
(**)where type SigStr		= StaticObjectsCore.Sig'


    (* Import *)

    type Mod			= StaticObjectsCore.Mod
    type TyNameSet		= StaticObjectsCore.TyNameSet
    type TyVarSet		= StaticObjectsCore.TyVarSet
    type Realisation		= Type.Realisation

    type Sig			= StaticObjectsModule.Sig
    type FunSig			= StaticObjectsModule.FunSig


    (* Recursive import *)

    structure Sig :		sig val matches : (Sig * Sig -> bool) ref end
    structure FunSig :		sig val matches : (FunSig * FunSig -> bool) ref end


    (* Operations *)

    val tyvarsVE :		ValEnv -> TyVarSet
    val tyvarsM :		Mod    -> TyVarSet
    val tyvars :		Env    -> TyVarSet
    val tynamesVE :		ValEnv -> TyNameSet
    val tynamesTE :		TyEnv  -> TyNameSet
    val tynamesSE :		StrEnv -> TyNameSet
    val tynamesG :		SigEnv -> TyNameSet
    val tynamesM :		Mod    -> TyNameSet
    val tynames :		Env    -> TyNameSet
    val undetermined :		Env    -> bool StampMap.map
    val undeterminedM :		Mod    -> bool StampMap.map

    val isWellFormed :		Env -> bool

    val Clos :			ValEnv -> ValEnv
    val maximiseEquality :	TyEnv * ValEnv -> TyEnv * ValEnv
    val realise :		Realisation -> Env -> Env
    val realiseM :		Realisation -> Mod -> Mod

    val enriches :		Env * Env -> bool
    val enrichesM :		Mod * Mod -> bool
    val equalsVE :		ValEnv * ValEnv -> bool
end;
