(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environment for binding analysis
 *)

structure BindingEnv : BINDING_ENV =
struct
  (* Inheritance *)

  structure GenericEnv =
      GenericEnvFn(
        open BindingObjectsCore
        type ValStr = IdStatus
        type TyStr  = ValEnv
        fun unEnv(Env E) = E
      )

  open GenericEnv
end;
