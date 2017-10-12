(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML types
 *
 * Definition, Section 4.2 and 4.4
 *
 * Notes:
 * - See StaticObjectsCore.sml
 * - Care has to be taken to clone types at the proper places.
 * - Substitution creates a clone, but shares undetermined types.
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
  type TypeFcn          = StaticObjectsCore.TypeFcn

  type 'a TyVarMap      = 'a TyVarMap.map
  type 'a TyNameMap     = 'a TyNameMap.map
 

  (* Types [Section 4.2 and 5.2] *)

  type Substitution     = Type TyVarMap                         (* [mu] *)
  type Realisation      = TypeFcn TyNameMap                     (* [phi] *)


  (* Operations *)

  val guess                : bool -> Type
  val invent               : bool -> Type
  val fromTyVar            : TyVar -> Type
  val fromRowType          : RowType -> Type
  val fromFunType          : FunType -> Type
  val fromConsType         : ConsType -> Type
  val fromOverloadingClass : OverloadingClass -> Type
  val fromTupleType        : Type list -> Type

  exception Type
  val determined           : Type -> StaticObjectsCore.Type'
  val toTyVar              : Type -> TyVar     (* raises Type *)
  val toRowType            : Type -> RowType   (* raises Type *)
  val toFunType            : Type -> FunType   (* raises Type *)
  val toConsType           : Type -> ConsType  (* raises Type *)

  val isOverloaded         : Type -> bool
  val tyname               : Type -> TyName
  val equals               : Type * Type -> bool (* raises Type *)

  val substitute           : Substitution -> Type -> Type
  val realise              : Realisation  -> Type -> Type
  val determine            : Type StampMap.map -> Type -> unit

  val tyvars               : Type -> TyVarSet
  val tynames              : Type -> TyNameSet
  val undetermined         : Type -> bool StampMap.map
  val admitsEquality       : Type -> bool (* raises Type *)

  exception Unify
  exception Flexible
  val unify                : Type * Type -> unit (* raises Unify *)
  val resolve              : Type -> unit        (* raises Flexible *)


  (* Operations on rows *)

  val emptyRow             : RowType
  val insertRow            : RowType * Lab * Type -> RowType
  val guessRow             : unit -> RowType
  val rowFromList          : (Lab * Type) list -> RowType
  val findLab              : RowType * Lab -> Type option
end;
