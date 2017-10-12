(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environments of the dynamic semantics of the core
 *
 * Definition, Sections 6.3 and 6.6
 *)

signature DYNAMIC_ENV =
sig
  (* Inheritance *)

  include GENERIC_ENV
    where type Env    = DynamicObjectsCore.Env
      and type ValStr = DynamicObjectsCore.ValStr
      and type TyStr  = DynamicObjectsCore.ValEnv


  (* Operations *)

  val Rec : ValEnv -> ValEnv
end;
