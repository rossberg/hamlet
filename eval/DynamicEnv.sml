(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML environments of the dynamic semantics of the core
 *
 * Definition, Sections 6.3 and 6.6
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

structure DynamicEnv :> DYNAMIC_ENV =
struct
    (* Inheritance *)

    structure GenericEnv = GenericEnvFn(open DynamicObjectsCore
					type TyStr = ValEnv
					type ModStr = Mod
					type SigStr = Int'
					fun env(Struct E)  = SOME E
					  | env(Functor _) = NONE
					fun unEnv(Env E) = E)
    open GenericEnv
    open DynamicObjectsCore


    (* Unrolling [Section 6.6] *)

    fun Rec VE =
	    VIdMap.map
		(fn (FcnClosure(match',E',VE'), IdStatus IdStatus.v) =>
		    (FcnClosure(match',E',VE), IdStatus IdStatus.v)
		  | valstr => valstr
		) VE
end;
