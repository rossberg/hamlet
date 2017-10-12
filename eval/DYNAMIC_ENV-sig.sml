(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML environments of the dynamic semantics of the core
 *
 * Definition, Sections 6.3 and 6.6
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

signature DYNAMIC_ENV =
sig
    (* Inheritance *)

    include GENERIC_ENV
    where type Env	= DynamicObjectsCore.Env
(**)where type ValStr	= DynamicObjectsCore.ValStr
(**)where type TyStr	= DynamicObjectsCore.ValEnv
(**)where type ModStr	= DynamicObjectsCore.Mod
(**)where type SigStr	= DynamicObjectsCore.Int'


    (* Operations *)

    val Rec :		ValEnv -> ValEnv
end;
