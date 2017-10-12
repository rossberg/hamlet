(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML objects for binding analysis of the core
 *
 * Definition, Section 2.9
 *
 * Notes:
 * - The "syntactic restrictions" defined in the Definition are not purely
 *   syntactic. E.g. the restriction that valbinds may not bind the same vid
 *   twice [2nd bullet] cannot be checked without proper binding analysis
 *   that computes identifier status.
 * - Also, checking of type variable shadowing [last bullet] is a global
 *   check dependent on context. Moreover, it requires the transformation from
 *   Section 4.6 to be done first.
 *)

structure BindingObjectsCore =
struct
  (* Import *)

  type 'a VIdMap   = 'a IdsCore.VIdMap
  type 'a TyConMap = 'a IdsCore.TyConMap
  type 'a StrIdMap = 'a IdsCore.StrIdMap

  type IdStatus = IdStatus.IdStatus
  type TyVarSet = TyVarSet.set


  (* Types *)

  type ValStr     = IdStatus
  type ValEnv     = IdStatus VIdMap
  type TyStr      = ValEnv
  type TyEnv      = ValEnv TyConMap
  datatype Env    = Env of StrEnv * TyEnv * ValEnv
  withtype StrEnv = Env StrIdMap

  type Context    = TyVarSet * Env
end;
