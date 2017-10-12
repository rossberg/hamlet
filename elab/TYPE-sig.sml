(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML types
 *
 * Definition, Section 4.2 and 4.4
 * + RFC: Record extension
 * + RFC: First-class modules
 *
 * Notes:
 *   - Types are references so that unification can work via side effects.
 *     We need links (forwards) to unify two type variables.
 *   - Types built bottom-up have to be `normalised' to induce the required
 *     sharing on type variables.
 *   - Care has to be taken to clone types at the proper places.
 *   - Undetermined types are represented separately from type variables.
 *     They carry an additional time stamp to detect attempts of referring
 *     types not in scope. The time stamps are also used to prevent invalid
 *     unification with skolem types (i.e. newer type names) during signature
 *     matching. Time stamps are propagated during unification.
 *   - Substitution creates a clone, but shares undetermined types.
 *   - To represent overloaded type (variables), we add a special type.
 *   - Record types may contain a row variable to represent open record types
 *     (which appear during type inference). Flexible rows have to carry an
 *     equality flag and time stamp to properly propagate information enforced
 *     by unification when extending a row.
 *)

signature TYPE =
sig
    (* Import types *)

    type Lab              = Lab.Lab
    type TyVar            = StaticObjectsCore.TyVar
    type TyVarSet         = StaticObjectsCore.TyVarSet
    type TyName           = StaticObjectsCore.TyName
    type TyNameSet        = StaticObjectsCore.TyNameSet
    type OverloadingClass = StaticObjectsCore.OverloadingClass
    type Type             = StaticObjectsCore.Type
    type RowType          = StaticObjectsCore.RowType
    type FunType          = StaticObjectsCore.FunType
    type ConsType         = StaticObjectsCore.ConsType
    type PackType         = StaticObjectsCore.PackType
    type TypeFcn          = StaticObjectsCore.TypeFcn
    type Sig'             = StaticObjectsCore.Sig'

    type 'a TyVarMap      = 'a TyVarMap.map
    type 'a TyNameMap     = 'a TyNameMap.map
 
    
    (* Types [Section 4.2 and 5.2] *)

    type Substitution = Type TyVarMap				(* [mu] *)
    type Realisation  = TypeFcn TyNameMap			(* [phi] *)


    (* Recursive import *)

    structure Sig :
    sig
	val tyvars :		(Sig' -> TyVarSet) ref
	val tynames :		(Sig' -> TyNameSet) ref
	val undetermined :	(Sig' -> bool StampMap.map) ref
	val realise :		(Realisation -> Sig' -> Sig') ref
	val matches :		(Sig' * Sig' -> bool) ref
    end

    (* Operations *)

    val guess :			bool -> Type
    val invent :		bool -> Type
    val fromTyVar :		TyVar -> Type
    val fromRowType :		RowType -> Type
    val fromFunType :		FunType -> Type
    val fromConsType :		ConsType -> Type
    val fromPackType :		PackType -> Type
    val fromOverloadingClass :	OverloadingClass -> Type

    val range :			Type -> Type
    val tyname :		Type -> TyName
    val equals :		Type * Type -> bool

    val substitute :		Substitution -> Type -> Type
    val realise :		Realisation  -> Type -> Type
    val determine :		Type StampMap.map -> Type -> Type

    val tyvars :		Type -> TyVarSet
    val tynames :		Type -> TyNameSet
    val undetermined :		Type -> bool StampMap.map
    val admitsEquality :	Type -> bool
    val isOverloaded :		Type -> bool

    exception Unify
    exception Flexible
    val unify :			Type * Type -> unit	(* Unify *)
    val resolve :		Type -> unit		(* Flexible *)


    (* Operations on rows *)

    val emptyRow :		RowType
    val singletonRow :		Lab * Type -> RowType
    val insertRow :		RowType * Lab * Type -> RowType
    val guessRow :		unit -> RowType
    val findLab :		RowType * Lab -> Type option
    val normalizeRow :		RowType -> unit
end;
