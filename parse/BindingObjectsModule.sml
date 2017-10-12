(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML objects for binding analysis of modules
 *
 * Definition, Sections 2.9 and 3.5
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *
 * Notes:
 *   - The "syntactic restrictions" defined in the Definition are not purely
 *     syntactic. E.g. the restriction that valbinds may not bind the same vid
 *     twice [2nd bullet] cannot be checked without proper binding analysis,
 *     to compute identifier status.
 *)

structure BindingObjectsModule =
struct
    (* Import *)

    type 'a SigIdMap	= 'a IdsModule.SigIdMap

    type Env		= BindingObjectsCore.Env
    datatype Mod	= datatype BindingObjectsCore.Mod

    (* Types *)

    type SigEnv		= exn SigIdMap
    type Basis		= Env

    (* Recursive export *)

    exception Fct	of Mod
    exception Sig	of Mod
end;
