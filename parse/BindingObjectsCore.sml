(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML objects for binding analysis of the core
 *
 * Definition, Section 2.9
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *
 * Notes:
 *   - The "syntactic restrictions" defined in the Definition are not purely
 *     syntactic. E.g. the restriction that valbinds may not bind the same vid
 *     twice [2nd bullet] cannot be checked without proper binding analysis,
 *     to compute identifier status.
 *   - Also, checking of type variable shadowing [last bullet] is a global
 *     check dependent on context. Moreover, it requires the transformation from
 *     Section 4.6 to be done first.
 *)

structure BindingObjectsCore =
struct
    (* Import *)

    type 'a VIdMap	= 'a IdsCore.VIdMap
    type 'a TyConMap	= 'a IdsCore.TyConMap
    type 'a StrIdMap	= 'a IdsCore.StrIdMap
    type 'a SigIdMap	= 'a IdsModule.SigIdMap

    type IdStatus	= IdStatus.IdStatus
    type TyVarSet	= TyVarSet.set

    (* Recursive import *)

    type Sig'		= exn
    

    (* Types *)

    type ValStr		= IdStatus
    type ValEnv		= IdStatus VIdMap
    type TyStr		= ValEnv
    type TyEnv		= ValEnv TyConMap
    type SigEnv		= Sig' SigIdMap
    datatype Env	= Env of SigEnv * StrEnv * TyEnv * ValEnv
    and      Mod	= Struct of Env | Functor of exn
    withtype StrEnv	= Mod StrIdMap

    type Context	= TyVarSet * Env
end;
