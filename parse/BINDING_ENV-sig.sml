(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environment for binding analysis
 *)

signature BINDING_ENV =
sig
  (* Inheritance *)

  include GENERIC_ENV
    where type Env    = BindingObjectsCore.Env
      and type ValStr = BindingObjectsCore.IdStatus
      and type TyStr  = BindingObjectsCore.ValEnv
end;
