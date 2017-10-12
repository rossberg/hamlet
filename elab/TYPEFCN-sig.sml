(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type functions
 *
 * Definition, Section 4.2, 4.4, and 4.8
 *
 * Note:
 *  Application copies the type (except free type variables).
 *)

signature TYPEFCN =
sig
  (* Import *)

  type Type        = StaticObjectsCore.Type
  type TypeFcn     = StaticObjectsCore.TypeFcn
  type TyVar       = StaticObjectsCore.TyVar
  type TyName      = StaticObjectsCore.TyName
  type TyVarSet    = StaticObjectsCore.TyVarSet
  type TyNameSet   = StaticObjectsCore.TyNameSet
  type Realisation = Type.Realisation


  (* Operations *)

  val fromTyName     : TyName  -> TypeFcn
  val toTyName       : TypeFcn -> TyName (* raises Type.Type *)
  val isTyName       : TypeFcn -> bool

  val arity          : TypeFcn -> int
  val admitsEquality : TypeFcn -> bool

  val tyvars         : TypeFcn -> TyVarSet
  val tynames        : TypeFcn -> TyNameSet
  val undetermined   : TypeFcn -> bool StampMap.map
  val normalise      : TypeFcn -> TypeFcn
  val rename         : TypeFcn -> TypeFcn

  val equals         : TypeFcn * TypeFcn -> bool

  exception Apply
  val apply          : Type list * TypeFcn -> Type (* raises Apply *)

  val realise        : Realisation -> TypeFcn -> TypeFcn
end;
