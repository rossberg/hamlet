(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML objects of the dynamic semantics of the core
 *
 * Definition, Sections 6.2 and 6.3
 *
 * Notes:
 * - Basic values are just named by strings.
 * - Env is modelled a a datatype to deal with type recursion.
 * - We call the domain type of value environments ValStr.
 *)

structure DynamicObjectsCore =
struct
  (* Import *)

  type VId         = IdsCore.VId
  type 'a LabMap   = 'a IdsCore.LabMap
  type 'a VIdMap   = 'a IdsCore.VIdMap
  type 'a TyConMap = 'a IdsCore.TyConMap
  type 'a StrIdMap = 'a IdsCore.StrIdMap

  type 'a AddrMap  = 'a AddrMap.map

  type IdStatus    = IdStatus.IdStatus

  type Match       = SyntaxCore.Match


  (* Simple objects [Section 6.2] *)

  type Addr   = Addr.Addr                                       (* [a] *)
  type ExName = ExName.ExName                                   (* [en] *)
  type BasVal = string                                          (* [b] *)
  type SVal   = SVal.SVal                                       (* [sv] *)

  exception FAIL

  (* Compound objects [Section 6.3] *)

  datatype Val =                                                (* [v] *)
      Assign
    | SVal       of SVal
    | BasVal     of BasVal
    | VId        of VId
    | VIdVal     of VId * Val
    | ExVal      of ExVal
    | Record     of Record
    | Addr       of Addr
    | FcnClosure of FcnClosure

  and ExVal =                                                   (* [e] *)
      ExName     of ExName
    | ExNameVal  of ExName * Val

  and Env =
      Env of StrEnv * TyEnv * ValEnv                            (* [E] *)

  withtype
      Record     = Val LabMap                                   (* [r] *)
  and FcnClosure = Match * Env * (Val * IdStatus) VIdMap
  and StrEnv     = Env StrIdMap                                 (* [SE] *)
  and TyEnv      = ((Val * IdStatus) VIdMap) TyConMap           (* [TE] *)
  and ValEnv     = (Val * IdStatus) VIdMap                      (* [VE] *)
  and ValStr     = Val * IdStatus

  type Pack      = ExVal * Source.loc                           (* [p] *)
  type Mem       = Val AddrMap                                  (* [mem] *)
  type ExNameSet = ExNameSet.set                                (* [ens] *)
  type State     = Mem * ExNameSet                              (* [s] *)

  exception Pack of Pack                                        (* [p] *)
end;
