(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML objects of the static semantics of modules
 *
 * Definition, Section 5.1
 *)

structure StaticObjectsModule =
struct
  (* Import *)

  type 'a SigIdMap = 'a IdsModule.SigIdMap
  type 'a FunIdMap = 'a IdsModule.FunIdMap

  type Env       = StaticObjectsCore.Env
  type TyNameSet = StaticObjectsCore.TyNameSet


  (* Compound objects [Section 5.1] *)

  type Sig    = TyNameSet * Env                                 (* [Sigma] *)
  type FunSig = TyNameSet * (Env * Sig)                         (* [Phi] *)

  type SigEnv = Sig SigIdMap                                    (* [G] *)
  type FunEnv = FunSig FunIdMap                                 (* [F] *)
  type Basis  = TyNameSet * FunEnv * SigEnv * Env               (* [B] *)
end;
