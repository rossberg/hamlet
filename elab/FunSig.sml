(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML functor signatures
 *
 * Definition, Sections 5.1 and 5.4
 *)

structure FunSig :> FUNSIG =
struct
  (* Import types *)

  open StaticObjectsCore
  open StaticObjectsModule


  (* Type variable and type name extraction [Section 4.2] *)

  fun tyvars(T, (E, Sigma)) =
      TyVarSet.union(StaticEnv.tyvars E, Sig.tyvars Sigma)

  fun tynames(T, (E, Sigma)) =
      TyNameSet.difference(
        TyNameSet.union(StaticEnv.tynames E, Sig.tynames Sigma), T)

  fun undetermined(T, (E, Sigma)) =
      StampMap.unionWith #2 (StaticEnv.undetermined E, Sig.undetermined Sigma)
end;
