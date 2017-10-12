(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML signatures
 *
 * Definition, Sections 5.1, 5.3, and 5.6
 *)

signature SIG =
sig
  (* Import types *)

  type TyVarSet    = StaticObjectsCore.TyVarSet
  type TyNameSet   = StaticObjectsCore.TyNameSet
  type Env         = StaticObjectsCore.Env
  type Sig         = StaticObjectsModule.Sig
  type Realisation = Type.Realisation


  (* Operations *)

  val tyvars       : Sig -> TyVarSet
  val tynames      : Sig -> TyNameSet
  val undetermined : Sig -> bool StampMap.map

  val rename       : Sig -> Sig

  exception Match
  val match        : Env * Sig -> Env * Realisation (* raises Match *)
end;
