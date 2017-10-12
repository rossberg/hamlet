(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML objects of the static semantics of modules
 *
 * Definition, Section 5.1
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

structure StaticObjectsModule =
struct
    (* Import *)

    type 'a SigIdMap	= 'a IdsModule.SigIdMap

    type Env		= StaticObjectsCore.Env
    type TyNameSet	= StaticObjectsCore.TyNameSet
    datatype Mod	= datatype StaticObjectsCore.Mod


    (* Compound objects [Section 5.1; RFC: Higher-order functors;
     *                                RFC: Nested signatures] *)

    type Sig		= TyNameSet * Mod			(* [Sigma] *)
    type FunSig		= TyNameSet * (Mod * Sig)		(* [Phi] *)

    type SigEnv		= exn SigIdMap				(* [G] *)
    type Basis		= TyNameSet * Env			(* [B] *)


    (* Recursive export *)

    exception Fct	of FunSig
    exception Sig	of Sig
end;
