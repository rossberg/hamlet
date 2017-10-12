(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environments of the static semantics of the core
 *
 * Definition, Sections 4.2, 4.3, 4.8, 4.9, and 5.5
 *
 * Notes: see StaticObjectsCore.sml
 *)

structure StaticEnv :> STATIC_ENV =
struct
  (* Inheritance *)

  structure GenericEnv =
      GenericEnvFn(open StaticObjectsCore; fun unEnv(Env E) = E)
  open GenericEnv


  (* Import *)

  open StaticObjectsCore

  type Realisation = Type.Realisation


  (* Further modifications [Section 4.3] *)

  infix TEplus
  fun TE' TEplus (Env(SE, TE, VE)) =
      Env(SE, TyConMap.unionWith #2 (TE', TE), VE)


  (* Type variable and type name set [Section 4.2] *)

  fun collect(empty, union, collectTypeScheme, collectTypeFcn) =
      let
        fun collect(Env(SE, TE, VE)) =
            union(union(collectSE SE, collectTE TE), collectVE VE)

        and collectVE(VE : ValEnv) =
            VIdMap.foldl
              (fn((sigma, is), S) => union(S, collectTypeScheme sigma))
              empty VE

        and collectTE(TE : TyEnv) =
            TyConMap.foldl
              (fn((theta, VE), S) =>
                union(union(S, collectTypeFcn theta), collectVE VE))
              empty TE

        and collectSE(SE : StrEnv) =
            StrIdMap.foldl (fn(E, S) => union(S, collect E)) empty SE
      in
        (collect, collectVE, collectTE, collectSE)
      end

  val (tyvars, tyvarsVE, tyvarsTE, tyvarsSE) =
      collect(TyVarSet.empty, TyVarSet.union, TypeScheme.tyvars, TypeFcn.tyvars)

  val (tynames, tynamesVE, tynamesTE, tynamesSE) =
      collect(TyNameSet.empty, TyNameSet.union,
        TypeScheme.tynames, TypeFcn.tynames)

  val (undetermined, undeterminedVE, undeterminedTE, undeterminedSE) =
      collect(StampMap.empty, StampMap.unionWith #2,
        TypeScheme.undetermined, TypeFcn.undetermined)


  (* Well-formedness [Section 4.9] *)

  fun isWellFormedTyStr (theta, VE) =
      VIdMap.isEmpty VE orelse TypeFcn.isTyName theta

  fun isWellFormedTE TE = TyConMap.all isWellFormedTyStr TE

  fun isWellFormedSE SE = StrIdMap.all isWellFormed SE

  and isWellFormed (Env(SE, TE, VE)) =
      isWellFormedTE TE andalso isWellFormedSE SE


  (* Closure [Section 4.8] *)

  fun Clos VE = VIdMap.map (fn((_, tau), is) => (TypeScheme.Clos tau, is)) VE


  (* Realisation [Section 5.2] *)

  fun realise phi (Env(SE, TE, VE)) =
      Env(realiseSE phi SE, realiseTE phi TE, realiseVE phi VE)

  and realiseVE phi VE =
      VIdMap.map (fn(sigma, is) => (TypeScheme.realise phi sigma, is)) VE

  and realiseTE phi TE =
      TyConMap.map
        (fn(theta, VE) => (TypeFcn.realise phi theta, realiseVE phi VE)) TE

  and realiseSE phi SE =
      StrIdMap.map (realise phi) SE


  (* Maximise equality of a type environment [Section 4.9] *)

  fun respectsEqualityValStr((alphas, ref(FunType(tau,  _))), is) =
        TypeFcn.admitsEquality(alphas, tau)
    | respectsEqualityValStr _ = true

  fun respectsEquality((alphas, tau), VE) =
      let
        val t = #2(Type.toConsType tau)
      in
        not(TyName.admitsEquality t) orelse
        TyName.toString t = "ref" orelse
        VIdMap.all respectsEqualityValStr VE
      end

  fun maximiseEquality TE =
      let
        fun check(theta, VE) =
            respectsEquality (theta, VE) orelse
            (TyName.removeEquality(TypeFcn.toTyName theta); false)
      in
        while not(TyConMap.all check TE) do ()
      end


  (* Abstraction of a type environment [Section 4.9] *)

  fun AbsTE(TE) = TyConMap.map (fn(theta, VE) => (theta, VIdMap.empty)) TE

  fun Abs(TE, E) =
      let
        val ts =
            TyConMap.foldl
              (fn((theta, VE), ts) => (TypeFcn.toTyName theta)::ts) [] TE
        val phi =
            List.foldl
              (fn(t, phi) =>
                TyNameMap.insert(phi, t, TypeFcn.fromTyName(TyName.Abs t)))
              TyNameMap.empty ts
      in
        realise phi (AbsTE(TE) TEplus E)
      end


  (* Disjointness *)

  fun disjoint(Env(SE1, TE1, VE1), Env(SE2, TE2, VE2)) =
      StrIdMap.disjoint(SE1, SE2) andalso
      TyConMap.disjoint(TE1, TE2) andalso
      VIdMap.disjoint(VE1, VE2)


  (* Enrichment [Section 5.5] *)

  fun equalsVE(VE1, VE2) =
      VIdMap.numItems VE1 = VIdMap.numItems VE2 andalso
      VIdMap.alli
        (fn(vid, (sigma1, is1)) =>
          case VIdMap.find(VE2, vid) of
            NONE => false
          | SOME(sigma2, is2) =>
              TypeScheme.equals(sigma1, sigma2) andalso is1 = is2
        ) VE1

  fun enriches(Env(SE1, TE1, VE1), Env(SE2, TE2, VE2)) =
      enrichesSE(SE1, SE2) andalso
      enrichesTE(TE1, TE2) andalso
      enrichesVE(VE1, VE2)

  and enrichesSE(SE1, SE2) =
      StrIdMap.alli
        (fn(strid, E2) =>
          case StrIdMap.find(SE1, strid) of
            NONE    => false
          | SOME E1 => enriches(E1, E2)
        ) SE2

  and enrichesTE(TE1, TE2) =
      TyConMap.alli
        (fn(tycon, tystr2) =>
          case TyConMap.find(TE1, tycon) of
            NONE        => false
          | SOME tystr1 => enrichesTyStr(tystr1, tystr2)
        ) TE2

  and enrichesVE(VE1, VE2) =
      VIdMap.alli
        (fn(vid, valstr2) =>
          case VIdMap.find(VE1, vid) of
            NONE         => false
          | SOME valstr1 => enrichesValStr(valstr1, valstr2)
        ) VE2

  and enrichesTyStr((theta1, VE1), (theta2, VE2)) =
      TypeFcn.equals(theta1, theta2) andalso
      (VIdMap.isEmpty VE2 orelse equalsVE(VE1, VE2))

  and enrichesValStr((sigma1, is1), (sigma2, is2)) =
      TypeScheme.generalises(sigma1, sigma2) andalso
      IdStatus.generalises(is1, is2)
end;
