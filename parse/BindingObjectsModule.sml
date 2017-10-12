(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML objects for binding analysis of modules
 *
 * Definition, Sections 2.9 and 3.5
 *
 * Notes:
 * - The "syntactic restrictions" defined in the Definition are not purely
 *   syntactic. E.g. the restriction that valbinds may not bind the same vid
 *   twice [2nd bullet] cannot be checked without proper binding analysis
 *   that computes identifier status.
 *)

structure BindingObjectsModule =
struct
  (* Import *)

  type 'a SigIdMap = 'a IdsModule.SigIdMap
  type 'a FunIdMap = 'a IdsModule.FunIdMap

  type Env = BindingObjectsCore.Env


  (* Types *)

  type SigEnv = Env SigIdMap
  type FunEnv = Env FunIdMap
  type Basis  = FunEnv * SigEnv * Env
end;
