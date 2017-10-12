(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML objects of the static semantics
 *
 * Definition, Sections 4.1, 4.2, and Appendix E
 *
 * Notes:
 * - Unification is destructive, hence types are wrapped in references.
 * - Undetermined types are represented separately from type variables.
 *   They carry an additional time stamp to detect attempts of referring to
 *   types not in scope. The time stamps are also used to prevent invalid
 *   unification with skolem types (i.e. newer type names) during signature
 *   matching. Time stamps are propagated during unification.
 * - When undetermined types get determined, they are replaced by forward links.
 * - To represent overloaded type (variables), we also add a special type.
 * - Record types may contain a row variable to represent open record types
 *   (which appear during type inference). Flexible rows have to carry an
 *   equality flag and time stamp to properly propagate information enforced
 *   by unification when extending a row.
 * - Env is modelled by a datatype to deal with type recursion.
 * - We call the domain type of value environments ValStr.
 *)

structure StaticObjectsCore =
struct
  (* Import *)

  type 'a LabMap   = 'a IdsCore.LabMap
  type 'a VIdMap   = 'a IdsCore.VIdMap
  type 'a TyConMap = 'a IdsCore.TyConMap
  type 'a StrIdMap = 'a IdsCore.StrIdMap


  (* Simple objects [Section 4.1 and Appendix E] *)

  type TyVar    = TyVar.TyVar                                      (* [alpha] *)
  type TyName   = TyName.TyName                                    (* [t] *)
  type IdStatus = IdStatus.IdStatus                                (* [is] *)

  type OverloadingClass = OverloadingClass.OverloadingClass        (* [O] *)


  (* Compound objects [Section 4.2] *)

  type RowVar = {eq : bool, time : Stamp.stamp}                    (* [r] *)

  datatype Type' =                                                 (* [tau] *)
      TyVar        of TyVar
    | RowType      of RowType
    | FunType      of FunType
    | ConsType     of ConsType
    | Undetermined of {stamp : Stamp.stamp, eq : bool, time : Stamp.stamp}
    | Overloaded   of OverloadingClass
    | Determined   of Type
  withtype Type = Type' ref
  and RowType   = Type' ref LabMap * RowVar option                 (* [rho] *)
  and FunType   = Type' ref * Type' ref
  and ConsType  = Type' ref list * TyName

  type TypeFcn    = TyVar list * Type                              (* [theta] *)
  type TypeScheme = TyVar list * Type                              (* [sigma] *)

  datatype Env = Env of StrEnv * TyEnv * ValEnv                    (* [E] *)
  withtype
      StrEnv = Env StrIdMap                                        (* [SE] *)
  and TyEnv  = (TypeFcn * (TypeScheme * IdStatus) VIdMap) TyConMap (* [TE] *)
  and ValEnv = (TypeScheme * IdStatus) VIdMap                      (* [VE] *)

  type ValStr = TypeScheme * IdStatus
  type TyStr  = TypeFcn * ValEnv

  type TyNameSet = TyNameSet.set                                   (* [T] *)
  type TyVarSet  = TyVarSet.set                                    (* [U] *)
  type Context   = TyNameSet * TyVarSet * Env                      (* [C] *)


  (* Exhaustiveness of patterns [Section 4.11] *)

  datatype Exhaustive = Exhaustive | NonExhaustive
end;
