(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML objects of the dynamic semantics of modules
 *
 * Definition, Section 7.2
 *)

structure DynamicObjectsModule =
struct
  (* Import *)

  type StrId            = IdsCore.StrId
  type 'a VIdMap        = 'a IdsCore.VIdMap
  type 'a TyConMap      = 'a IdsCore.TyConMap
  type 'a StrIdMap      = 'a IdsCore.StrIdMap
  type 'a SigIdMap      = 'a IdsModule.SigIdMap
  type 'a FunIdMap      = 'a IdsModule.FunIdMap

  type IdStatus = DynamicObjectsCore.IdStatus
  type Env              = DynamicObjectsCore.Env

  type StrExp           = SyntaxModule.StrExp


  (* Compound objects [Section 7.2] *)

  datatype Int = Int of StrInt * TyInt * ValInt                 (* [I] *)
  withtype
      StrInt = Int StrIdMap                                     (* [SI] *)
  and TyInt  = (IdStatus VIdMap) TyConMap                       (* [TI] *)
  and ValInt = IdStatus VIdMap                                  (* [VI] *)

  datatype FunctorClosure =
      FunctorClosure of (StrId * Int) * StrExp * (FunEnv * SigEnv * Env)
  withtype
      SigEnv    = Int SigIdMap                                  (* [G] *)
  and FunEnv    = FunctorClosure FunIdMap                       (* [F] *)

  type Basis    = FunEnv * SigEnv * Env                         (* [B] *)
  type IntBasis = SigEnv * Int                                  (* [IB] *)
end;
