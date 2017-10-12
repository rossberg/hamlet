(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML environment for binding analysis
 *)

signature BINDING_ENV =
sig
    (* Inheritance *)

    include GENERIC_ENV
    where type Env	= BindingObjectsCore.Env
(**)where type ValStr	= BindingObjectsCore.IdStatus
(**)where type TyStr	= BindingObjectsCore.ValEnv
(**)where type ModStr	= BindingObjectsCore.Mod
(**)where type SigStr	= BindingObjectsCore.Sig'
end;
