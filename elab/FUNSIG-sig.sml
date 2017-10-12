(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML functor signatures
 *
 * Definition, Sections 5.1 and 5.4
 *)

signature FUNSIG =
sig
  (* Import *)

  type TyVarSet  = StaticObjectsCore.TyVarSet
  type TyNameSet = StaticObjectsCore.TyNameSet
  type FunSig    = StaticObjectsModule.FunSig


  (* Operations *)

  val tyvars       : FunSig -> TyVarSet
  val tynames      : FunSig -> TyNameSet
  val undetermined : FunSig -> bool StampMap.map
end;
