(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML objects of the dynamic semantics of the core
 *
 * Definition, Sections 6.2, 6.3 and 7.2
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: First-class modules
 *
 * Notes:
 *   - Basic values are just named by strings.
 *   - Env is modelled by a datatype to deal with type recursion.
 *   - We call the domain type of value environments ValStr.
 *   - FunctorClosure is defined as exn, to recolve the mutual dependency.
 *)

structure DynamicObjectsCore =
struct
    (* Import *)

    type VId		= IdsCore.VId
    type 'a LabMap	= 'a IdsCore.LabMap
    type 'a VIdMap	= 'a IdsCore.VIdMap
    type 'a TyConMap	= 'a IdsCore.TyConMap
    type 'a StrIdMap	= 'a IdsCore.StrIdMap
    type 'a SigIdMap	= 'a IdsModule.SigIdMap

    type 'a AddrMap	= 'a AddrMap.map

    type IdStatus	= IdStatus.IdStatus

    type Match		= GrammarCore.Match

    (* Recursive import *)

    type Int'		= exn
    type FunctorClosure' = exn

    (* Simple objects [Section 6.2] *)

    type Addr		= Addr.Addr				(* [a] *)
    type ExName		= ExName.ExName				(* [en] *)
    type BasVal		= string				(* [b] *)
    type SVal		= SVal.SVal				(* [sv] *)

    exception FAIL

    (* Compound objects [Section 6.3] *)

    datatype Val	=					(* [v] *)
	  Assign
	| SVal		of SVal
	| BasVal	of BasVal
	| VId		of VId
	| VIdVal	of VId * Val
	| ExVal		of ExVal
	| Record	of Record
	| Addr		of Addr
	| FcnClosure	of FcnClosure
	| Mod		of Mod

    and ExVal		=					(* [e] *)
	  ExName	of ExName
	| ExNameVal	of ExName * Val

    and Env		= Env of SigEnv * StrEnv * TyEnv * ValEnv (* [E] *)

    and Mod		=					(* [M] *)
	  Struct	of Env
	| Functor	of FunctorClosure'

    and ValStatus	=					(* [vs] *)
	  IdStatus	of IdStatus
	| Vals		of Val * VId

    withtype Record	= Val LabMap				(* [r] *)
    and      FcnClosure	= Match * Env * (Val * ValStatus) VIdMap
    and      SigEnv	= Int' SigIdMap				(* [G] *)
    and      StrEnv	= Mod StrIdMap				(* [SE] *)
    and      TyEnv	= ((Val * ValStatus) VIdMap) TyConMap	(* [TE] *)
    and      ValEnv	= (Val * ValStatus) VIdMap		(* [VE] *)
    and      ValStr	= Val * ValStatus

    type     Pack	= ExVal					(* [p] *)
    type     Mem	= Val AddrMap				(* [mem] *)
    type     ExNameSet	= ExNameSet.set				(* [ens] *)
    type     State	= Mem * ExNameSet			(* [s] *)

    exception Pack	of Pack					(* [p] *)
end;
