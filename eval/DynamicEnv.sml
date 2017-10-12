(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environments of the dynamic semantics of the core
 *
 * Definition, Sections 6.3 and 6.6
 *)

structure DynamicEnv :> DYNAMIC_ENV =
struct
  (* Inheritance *)

  structure GenericEnv =
      GenericEnvFn(
        open DynamicObjectsCore
        type TyStr = ValEnv
        fun unEnv(Env E) = E
      )

  open GenericEnv
  open DynamicObjectsCore


  (* Unrolling [Section 6.6] *)

  fun Rec VE =
      VIdMap.map
        (fn(FcnClosure(match', E', VE'), IdStatus.v) =>
            (FcnClosure(match', E', VE), IdStatus.v)
          | valstr => valstr
        ) VE
end;
