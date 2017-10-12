(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML environment for binding analysis
 *)

structure BindingEnv : BINDING_ENV =
struct
    (* Inheritance *)

    structure GenericEnv = GenericEnvFn(open BindingObjectsCore
					type ValStr = IdStatus
					type TyStr  = ValEnv
					type ModStr = Mod
					type SigStr = Sig'
					fun env(Struct E)  = SOME E
					  | env(Functor _) = NONE
					fun unEnv(Env E) = E)
    open GenericEnv
end;
