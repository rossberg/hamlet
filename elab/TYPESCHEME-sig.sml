(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type schemes
 *
 * Definition, Section 4.2, 4.5, and 4.8
 *
 * Note:
 *   Instantiation copies a type (except free type variables).
 *   Closure does not!
 *)

signature TYPESCHEME =
sig
  (* Import *)

  type Type         = StaticObjectsCore.Type
  type TypeScheme   = StaticObjectsCore.TypeScheme
  type TyVar        = StaticObjectsCore.TyVar
  type TyName       = StaticObjectsCore.TyName
  type TyVarSet     = StaticObjectsCore.TyVarSet
  type TyNameSet    = StaticObjectsCore.TyNameSet

  type Substitution = Type.Substitution
  type Realisation  = Type.Realisation
  type 'a TyNameMap = 'a TyNameMap.map


  (* Operations *)

  val instance       : TypeScheme -> Type list * Type
  val Clos           : Type -> TypeScheme
  val ClosRestricted : TyVarSet -> Type -> TypeScheme
  val isClosed       : TypeScheme -> bool

  val tyvars         : TypeScheme -> TyVarSet
  val tynames        : TypeScheme -> TyNameSet
  val undetermined   : TypeScheme -> bool StampMap.map
  val normalise      : TypeScheme -> TypeScheme

  val generalises    : TypeScheme * TypeScheme -> bool
  val equals         : TypeScheme * TypeScheme -> bool

  val substitute     : Substitution -> TypeScheme -> TypeScheme
  val realise        : Realisation  -> TypeScheme -> TypeScheme
end;
