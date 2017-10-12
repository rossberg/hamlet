(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML objects of the static semantics
 *
 * Definition, Sections 4.1, 4.2, 5.1, and Appendix E
 * + RFC: Record extension
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: First-class modules
 *
 * Notes:
 *   - Types are references so that unification can work via side effects.
 *     We need links (forwards) to unify two type variables.
 *   - Undetermined types are represented separately from type variables.
 *     They carry an additional time stamp to detect attempts of referring
 *     types not in scope. The time stamps are also used to prevent invalid
 *     unification with skolem types (i.e. newer type names) during signature
 *     matching. Time stamps are propagated during unification.
 *   - To represent overloaded type (variables), we also add a special type.
 *   - Record types may contain a row variable to represent open record types
 *     (which appear during type inference). Flexible rows have to carry an
 *     equality flag and time stamp to properly propagate information enforced
 *     by unification when extending a row.
 *   - Types built bottom-up have to be `normalised' to induce the required
 *     sharing on type variables. Care has to be taken to clone types at the
 *     proper places.
 *   - Env is modelled by a datatype to deal with type recursion.
 *   - We call the domain type of value environments ValStr.
 *   - FunSig is defined as exn, to recolve the mutual dependency.
 *)

structure StaticObjectsCore =
struct
    (* Import *)

    type 'a LabMap	= 'a IdsCore.LabMap
    type 'a VIdMap	= 'a IdsCore.VIdMap
    type 'a TyConMap	= 'a IdsCore.TyConMap
    type 'a StrIdMap	= 'a IdsCore.StrIdMap
    type 'a SigIdMap	= 'a IdsModule.SigIdMap

    (* Recursive import *)

    type Sig'		= exn
    type FunSig'	= exn
    
    (* Simple objects [Section 4.1 and Appendix E] *)

    type TyVar		= TyVar.TyVar				(* [alpha] *)
    type TyName		= TyName.TyName				(* [t] *)
    type IdStatus	= IdStatus.IdStatus			(* [is] *)

    type OverloadingClass = OverloadingClass.OverloadingClass	(* [O] *)


    (* Compound objects [Section 4.2; RFC: Record extension;
     *                                RFC: Higher-order functors;
     *                                RFC: Nested signatures;
     *                                RFC: First-class modules] *)

    datatype Type'	=					(* [tau] *)
	  TyVar		of TyVar
	| RowType	of RowType
	| FunType	of FunType
	| ConsType	of ConsType
	| PackType      of PackType
	| Undetermined	of {stamp : Stamp.stamp, eq : bool, time : Stamp.stamp}
	| Overloaded	of OverloadingClass
	| Determined	of Type

    and RowType'	=					(* [rho] *)
	  FixedRow	of Type LabMap
	| FlexRow	of {fixed : Type LabMap, flex : RowType}
	| FreeRow	of {eq : bool, time : Stamp.stamp, excluded: LabSet.set}

    withtype Type	= Type' ref
    and      RowType	= RowType' ref
    and      FunType	= Type' ref * Type' ref
    and      ConsType	= Type' ref list * TyName
    and      PackType   = Sig'

    type     TypeFcn	= TyVar list * Type			(* [theta] *)
    type     TypeScheme	= TyVar list * Type			(* [sigma] *)

    datatype Env	= Env of SigEnv * StrEnv * TyEnv * ValEnv (* [E] *)

    and      Mod	=					(* [M] *)
	     Struct	of Env
	   | Functor	of FunSig'

    and      ValStatus	=					(* [vs] *)
	     IdStatus	of IdStatus
	   | TyName	of TyName
    
    withtype SigEnv	= Sig' SigIdMap				(* [G] *)
    and      StrEnv	= Mod StrIdMap				(* [SE] *)
    and      TyEnv	= (TypeFcn * (TypeScheme * ValStatus) VIdMap) TyConMap
								(* [TE] *)
    and      ValEnv	= (TypeScheme * ValStatus) VIdMap	(* [VE] *)
    type     ValStr	= TypeScheme * ValStatus
    type     TyStr	= TypeFcn * ValEnv

    type     TyNameSet	= TyNameSet.set				(* [T] *)
    type     TyVarSet	= TyVarSet.set				(* [U] *)
    type     Context	= TyNameSet * TyVarSet * Env		(* [C] *)
end;
