(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML objects of the dynamic semantics of modules
 *
 * Definition, Section 7.2
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

structure DynamicObjectsModule =
struct
    (* Import *)

    type StrId		= IdsCore.StrId
    type 'a VIdMap	= 'a IdsCore.VIdMap
    type 'a TyConMap	= 'a IdsCore.TyConMap
    type 'a StrIdMap	= 'a IdsCore.StrIdMap
    type 'a SigIdMap	= 'a IdsModule.SigIdMap

    type IdStatus	= DynamicObjectsCore.IdStatus
    type Env		= DynamicObjectsCore.Env
    type Mod		= DynamicObjectsCore.Mod

    type StrExp		= GrammarModule.StrExp


    (* Compound objects [Section 7.2] *)

    datatype Int	= Int of SigEnv * StrInt * TyInt * ValInt (* [I] *)
    and      ValIntStatus = IdStatus of IdStatus | f		(* [vis] *)
    withtype SigEnv	= exn SigIdMap				(* [G] *)
    and      StrInt	= Int StrIdMap				(* [SI] *)
    and      TyInt	= (ValIntStatus VIdMap) TyConMap	(* [TI] *)
    and      ValInt	= ValIntStatus VIdMap			(* [VI] *)

    type IntConstraint	= Int option				(* [IC] *)
    type Basis		= Env					(* [B] *)
    type FunctorClosure	= (StrId * Int) * (StrExp * IntConstraint) * Env


    (* Recursive export *)

    exception Fct	of FunctorClosure
    exception Sig	of Int
end;
