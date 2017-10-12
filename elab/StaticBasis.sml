(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML static basis and environments of modules
 *
 * Definition, Section 5.1
 *)

structure StaticBasis :> STATIC_BASIS =
struct
  (* Import *)

  open IdsCore
  open IdsModule
  open StaticObjectsCore
  open StaticObjectsModule


  (* Calculation of type variable and type name sets [Section 4.2] *)

  fun collect(empty, union, collectSig, collectFunSig, collectEnv) =
      let
        fun collectG(G : SigEnv) =
            SigIdMap.foldl (fn(Sigma, S) => union(S, collectSig Sigma)) empty G

        fun collectF(F : FunEnv) =
            FunIdMap.foldl (fn(Phi, S) => union(S, collectFunSig Phi)) empty F

        fun collect((T, F, G, E) : Basis) =
            union(union(collectF F, collectG G), collectEnv E)
      in
        (collect, collectF, collectG)
      end

  val (tyvars, tyvarsF, tyvarsG) =
      collect(TyVarSet.empty, TyVarSet.union,
        Sig.tyvars, FunSig.tyvars, StaticEnv.tyvars)

  val (tynames', tynamesF, tynamesG) =
      collect(TyNameSet.empty, TyNameSet.union,
        Sig.tynames, FunSig.tynames, StaticEnv.tynames)

  fun tynames(B as (T, F, G, E)) = TyNameSet.union(T, tynames' B)

  val (undetermined, undeterminedF, undeterminedG) =
      collect(StampMap.empty, StampMap.unionWith #2,
        Sig.undetermined, FunSig.undetermined, StaticEnv.undetermined)


  (* Injection [Sections 4.3 and 5.1] *)

  val empty = (TyNameSet.empty, FunIdMap.empty, SigIdMap.empty, StaticEnv.empty)

  fun fromTandE(T, E) = (T, FunIdMap.empty, SigIdMap.empty, E)
  fun fromTandF(T, F) = (T, F, SigIdMap.empty, StaticEnv.empty)
  fun fromTandG(T, G) = (T, FunIdMap.empty, G, StaticEnv.empty)


  (* Projections [Sections 4.3 and 5.1] *)

  fun Tof(T, F, G, E) = T
  fun Cof(T, F, G, E) = (T, TyVarSet.empty, E)


  (* Modifications [Sections 4.3 and 5.1] *)

  infix plus plusT oplusG oplusF oplusE oplusSE

  fun (T, F, G, E) plus (T', F', G', E') =
      ( TyNameSet.union(T, T'),
        FunIdMap.unionWith #2 (F, F'),
        SigIdMap.unionWith #2 (G, G'),
        StaticEnv.plus(E, E')
      )

  fun (T, F, G, E) plusT T' = (TyNameSet.union(T, T'), F, G, E)

  fun (T, F, G, E) oplusG G' =
      (TyNameSet.union(T, tynamesG G'), F, SigIdMap.unionWith #2 (G, G'), E)
  fun (T, F, G, E) oplusF F' =
      (TyNameSet.union(T, tynamesF F'), FunIdMap.unionWith #2 (F, F'), G, E)
  fun (T, F, G, E) oplusE E' =
      (TyNameSet.union(T, StaticEnv.tynames E'), F, G, StaticEnv.plus(E, E'))
  fun (T, F, G, E) oplusSE SE =
      (TyNameSet.union(T, StaticEnv.tynamesSE SE),
        F, G, StaticEnv.plusSE(E, SE))


  (* Application (lookup) [Sections 5.1 and 4.3] *)

  fun findStrId((T, F, G, E), strid) = StaticEnv.findStrId(E, strid)
  fun findSigId((T, F, G, E), sigid) = SigIdMap.find(G, sigid)
  fun findFunId((T, F, G, E), funid) = FunIdMap.find(F, funid)

  fun findLongStrId((T, F, G, E), longstrid) =
      StaticEnv.findLongStrId(E, longstrid)
  fun findLongTyCon((T, F, G, E), longtycon) =
      StaticEnv.findLongTyCon(E, longtycon)


  (* Disjointness *)

  fun disjoint((T1, F1, G1, E1), (T2, F2, G2, E2)) =
      FunIdMap.disjoint(F1, F2) andalso
      SigIdMap.disjoint(G1, G2) andalso
      StaticEnv.disjoint(E1, E2)


  (* Conversion to binding basis *)

  fun toBindingValEnv VE = VIdMap.map (fn(sigma, is) => is) VE
  fun toBindingTyEnv TE  = TyConMap.map (fn(theta, VE) => toBindingValEnv VE) TE
  fun toBindingStrEnv SE = StrIdMap.map toBindingEnv SE
  and toBindingEnv(Env(SE, TE, VE)) =
      BindingObjectsCore.Env(
        toBindingStrEnv SE, toBindingTyEnv TE, toBindingValEnv VE)

  fun toBindingSigEnv G = SigIdMap.map (fn(T, E) => toBindingEnv E) G
  fun toBindingFunEnv F =
      FunIdMap.map (fn(T, (E, (T', E'))) => toBindingEnv E') F
  fun toBindingBasis (T, F, G, E) =
      (toBindingFunEnv F, toBindingSigEnv G, toBindingEnv E)
end;
