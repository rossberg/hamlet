(*
 * (c) Andreas Rossberg 2013
 *
 * System F static semantics.
 *)

structure FStatic : F_STATIC =
struct
  open FSyntax

  type typ_env = kind VarMap.map         (* [D] *)
  type val_env = typ VarMap.map          (* [G] *)

  exception Error of string

  fun error s        = raise Error(s)
  fun errorVar(s, x) = error(s ^ " " ^ x)

  (* Substitution *)
  fun substTyp s (VarTyp(a)) =
        (case VarMap.find(s, a) of
          SOME t => t
        | NONE => VarTyp(a)
        )
    | substTyp s (VoidTyp)           = VoidTyp
    | substTyp s (UnitTyp)           = UnitTyp
    | substTyp s (ArrowTyp(t1, t2))  = ArrowTyp(substTyp s t1, substTyp s t2)
    | substTyp s (ProdTyp(t1, t2))   = ProdTyp(substTyp s t1, substTyp s t2)
    | substTyp s (SumTyp(t1, t2))    = SumTyp(substTyp s t1, substTyp s t2)
    | substTyp s (UnivTyp(a, k, t))  = UnivTyp(substBindTyp s (a, k, t))
    | substTyp s (ExistTyp(a, k, t)) = ExistTyp(substBindTyp s (a, k, t))
    | substTyp s (ExnTyp)            = ExnTyp
    | substTyp s (TagTyp(t))         = TagTyp(substTyp s t)
    | substTyp s (RefTyp(t))         = RefTyp(substTyp s t)
    | substTyp s (FunTyp(a, k, t))   = FunTyp(substBindTyp s (a, k, t))
    | substTyp s (AppTyp(t1, t2))    = AppTyp(substTyp s t1, substTyp s t2)
  and substBindTyp s (a, k, t) =
      let
        val a' = renameVar(a)
      in
        (a', k, substTyp (VarMap.insert(s, a, VarTyp(a'))))
      end

  (* Long-eta-normalize a type whose components are already long-eta-normal *)
  fun longEtaNormTyp(StarKind, t) = t
    | longEtaNormTyp(ArrowKind(_), t as FunTyp(_)) = t
    | longEtaNormTyp(ArrowKind(k1, k2), t) =
      let
        val a = inventVar()
      in
        FunTyp(a, k1, longEtaNormTyp(k, AppTyp(t, VarTyp(a))))
      end

  (* Beta-normalize a long-eta-normal type *)
  fun normTyp(VarTyp(a))         = VarTyp(a)
    | normTyp(VoidTyp)           = VoidTyp
    | normTyp(UnitTyp)           = UnitTyp
    | normTyp(ArrowTyp(t1, t2))  = ArrowTyp(normTyp t1, normTyp t2)
    | normTyp(ProdTyp(t1, t2))   = ProdTyp(normTyp t1, normTyp t2)
    | normTyp(SumTyp(t1, t2))    = SumTyp(normTyp t1, normTyp t2)
    | normTyp(UnivTyp(a, k, t))  = UnivTyp(a, k, normTyp t)
    | normTyp(ExistTyp(a, k, t)) = ExistTyp(a, k, normTyp t)
    | normTyp(ExnTyp)            = ExnTyp
    | normTyp(TagTyp(t))         = TagTyp(normTyp t)
    | normTyp(RefTyp(t))         = RefTyp(normTyp t)
    | normTyp(FunTyp(a, k, t))   = FunTyp(a, k, normTyp t)
    | normTyp(AppTyp(t1, t2))    =
        (case (normTyp t1, normTyp t2) of
          (FunTyp(a, k, t), t2') => substTyp (VarMap.singleton(a, t2')) t
        | (t1', t2') => AppTyp(t1', t2')
        )

  (* Compare two beta-long-eta-normal types *)
  fun eqNormTyp(VarTyp(a1), VarTyp(a2)) =
        a1 = a2
    | eqNormTyp(VoidTyp, VoidTyp) =
        true
    | eqNormTyp(UnitTyp, UnitTyp) =
        true
    | eqNormTyp(ArrowTyp(t11, t12), ArrowTyp(t21, t22)) =
        eqNormTyp(t11, t21) andalse eqNormTyp(t12, t22)
    | eqNormTyp(ProdTyp(t11, t12), ProdTyp(t21, t22)) =
        eqNormTyp(t11, t21) andalse eqNormTyp(t12, t22)
    | eqNormTyp(SumTyp(t11, t12), SumTyp(t21, t22)) =
        eqNormTyp(t11, t21) andalse eqNormTyp(t12, t22)
    | eqNormTyp(UnivTyp(a1, k1, t1), UnivTyp(a2, k2, t2)) =
        eqNormBind((a1, k1, t1), (a2, k2, t2))
    | eqNormTyp(ExistTyp(a1, k1, t1), ExistTyp(a2, k2, t2)) =
        eqNormBind((a1, k1, t1), (a2, k2, t2))
    | eqNormTyp(ExnTyp, ExnTyp) =
        true
    | eqNormTyp(TagTyp(t1), TagTyp(t2)) =
        eqNormTyp(t1, t2)
    | eqNormTyp(RefTyp(t1), RefTyp(t2)) =
        eqNormTyp(t1, t2)
    | eqNormTyp(FunTyp(a1, k1, t1), FunTyp(a2, k2, t2)) =
        eqNormBind((a1, k1, t1), (a2, k2, t2))
    | eqNormTyp(AppTyp(t11, t12), AppTyp(t21, t22)) =
        eqNormTyp(t11, t21) andalse eqNormTyp(t12, t22)
    | eqNormTyp(_, _) =
        false
  and eqNormBindTyp((a1, k1, t1), (a2, k2, t2)) =
      let
        val ta  = VarTyp(renameVar(a1))
        val t1' = substTyp (VarMap.singleton(a1, ta)) t1
        val t2' = substTyp (VarMap.singleton(a2, ta)) t2
      in
        k1 = k2 andalse eqNormTyp(t1', t2')
      end

  fun eqTyp(t1, t2) = eqNormTyp(normTyp t1, normTyp t2)


  fun elabTypAs(D, t, k) =
      if elabTyp(D, t) = k then () else error("kind mismatch")
  and elabTyp(D, VarTyp(a)) =
        (case VarMap.find(D, a) of
          SOME k => k
        | NONE => errorVar("unbound type variable", a)
        )
    | elabTyp(D, VoidTyp) =
        StarKind
    | elabTyp(D, UnitTyp) =
        StarKind
    | elabTyp(D, ArrowTyp(t1, t2)) =
        (elabTypAs(D, t1, StarKind); elabTypAs(D, t2, StarKind); StarKind)
    | elabTyp(D, ProdTyp(t1, t2)) =
        (elabTypAs(D, t1, StarKind); elabTypAs(D, t2, StarKind); StarKind)
    | elabTyp(D, SumTyp(t1, t2)) =
        (elabTypAs(D, t1, StarKind); elabTypAs(D, t2, StarKind); StarKind)
    | elabTyp(D, UnivTyp(a, k, t)) =
        (elabTypAs(VarMap.insert(D, a, k), t, StarKind); StarKind)
    | elabTyp(D, ExistTyp(a, k, t)) =
        (elabTypAs(VarMap.insert(D, a, k), t, StarKind); StarKind)
    | elabTyp(D, ExnTyp) =
        StarKind
    | elabTyp(D, TagTyp(t)) =
        (elabTypAs(D, t, StarKind); StarKind)
    | elabTyp(D, RefTyp(t)) =
        (elabTypAs(D, t, StarKind); StarKind)
    | elabTyp(D, FunTyp(a, k, t)) =
        ArrowKind(k, elabTyp(VarMap.insert(D, a, k), t))
    | elabTyp(D, AppTyp(t1, t2)) =
        (case elabTyp(D, t1) of
          ArrowKind(k2, k) => ( elabTypAs(D, t2, k2); k )
        | _ => error("AppTyp: arrow kind expected")
        )

  fun elabValAs(D, G, v, t) =
      if eqTyp(elabVal(D, G, v), t) then () else error("type mismatch")
  and elabVal(D, G, VarVal(x)) =
        (case VarMap.find(G, x) of
          SOME t => t
        | NONE => errorVar("unbound value variable", x)
        )
    | elabVal(D, G, UnitVal) =
        UnitTyp
    | elabVal(D, G, FunVal(x, t, e)) =
        ArrowTyp(t, elabExp(D, VarMap.insert(G, x, t), e))
    | elabVal(D, G, ProdVal(v1, v2)) =
        ProdTyp(elabVal(D, G, v1), elabVal(D, G, v2))
    | elabVal(D, G, SumVal(i, v, as_t)) =
      (
        elabTypAs(D, as_t, StarKind);
        (case normTyp as_t of
          SumTyp(t1, t2) =>
            elabValAs(D, G, v, case i of Fst => t1 | Snd => t2)
        | _ => error("SumVal: malformed type annotation")
        );
        as_t
      )
    | elabVal(D, G, RollVal(v, as_t)) =
      (
        elabTypAs(D, as_t, StarKind);
        case normTyp as_t of
          RecTyp(a, k, t') =>  (*TODO: higher kinds *)
            elabValAs(D, G, v, substTyp (VarMap.singleton(a, t)) t)
        | _ => error("RollVal: malformed type annotation");
        as_t
      )
    | elabVal(D, G, GenVal(a, k, e)) =
        UnivTyp(a, k, elabExp(VarMap.insert(D, a, k), G, e))
    | elabVal(D, G, PackVal(t, v, as_t)) =
      (
        elabTypAs(D, as_t, StarKind);
        (case normTyp as_t of
          ExistTyp(a, k, t') =>
          (
            elabTypAs(D, t, k);
            elabValAs(D, G, v, substTyp (VarMap.singleton(a, t)) t')
          )
        | _ => error("PackVal: malformed type annotation")
        );
        as_t
      )
    | elabVal(D, G, ExnVal(v1, v2)) =
        (case elabVal(D, G, v1) of
           TagTyp(t) => ( elabValAs(D, G, v, t); ExnTyp )
         | _ => error("ExnVal: ill-typed tag")
        )


  and elabVal(D, G, v) =
      case v of
        VarVal(x) =>
          (case VarMap.find(G, x) of
            SOME t => t
          | NONE => errorVar("unbound value variable", x)
          )
      | UnitVal =>
          UnitTyp
      | FunVal(x, t, e) =>
          ArrowTyp(t, elabExp(D, VarMap.insert(G, x, t), e))
      | ProdVal(v1, v2) =>
          ProdTyp(elabVal(D, G, v1), elabVal(D, G, v2))
      | SumVal(i, v, as_t) =>
        (
          elabTypAs(D, as_t, StarKind);
          case normTyp as_t of
            SumTyp(t1, t2) =>
              elabValAs(D, G, v, case i of Fst => t1 | Snd => t2)
          | _ => error("SumVal: malformed type annotation");
          as_t
        )
      | RollVal(v, as_t) =>
        (
          elabTypAs(D, as_t, StarKind);
          case normTyp as_t of
            RecTyp(a, k, t') =>  (*TODO: higher kinds *)
              elabValAs(D, G, v, substTyp (VarMap.singleton(a, t)) t)
          | _ => error("RollVal: malformed type annotation");
          as_t
        )
      | GenVal(a, k, e) =>
          UnivTyp(a, k, elabExp(VarMap.insert(D, a, k), G, e))
      | PackVal(t, v, as_t) =>
        (
          elabTypAs(D, as_t, StarKind);
          case normTyp as_t of
            ExistTyp(a, k, t') =>
            (
              elabTypAs(D, t, k);
              elabValAs(D, G, v, substTyp (VarMap.singleton(a, t)) t')
            )
          | _ => error("PackVal: malformed type annotation");
          as_t
        )
      | ExnVal(v1, v2) =>
          (case elabVal(D, G, v1) of
            TagTyp(t) => ( elabValAs(D, G, v, t); ExnTyp )
          | _ => error("ExnVal: ill-typed tag")
          )


  and elabExpAs(D, G, e, t) =
      if eqTyp(elabExp(D, G, e), t) then () else error("type mismatch")
  and elabExp(D, G, ValExp(e)) =
        elabVal(D, G, e)
    | elabExp(D, G, SeqExp(e1, e2)) =
        (elabExpAs(D, G, e1, UnitTyp); elabExp(D, G, e2))
    | elabExp(D, G, AppExp(e1, e2)) =
        (case elabExp(D, G, e1) of
          ArrowTyp(t2, t) => (elabExpAs(D, e2, t2); t)
        | _ => error("AppExp: arrow type expected")
        )
    | elabExp(D, G, ProjExp(i, e)) =
        (case elabExp(D, G, e) of
          ProdTyp(t1, t2) => (case i of Fst => t1 | Snd => t2)
        | _ => error("ProjExp: product type expected")
        )
    | elabExp(D, G, CaseExp(e, x1, e1, x2, e2)) =
        (case elabExp(D, G, e) of
          SumTyp(t1, t2) =>
          let
            val t = elabExp(D, VarMap.insert(G, x1, t1), e1)
          in
            elabExpAs(D, VarMap.insert(G, x2, t2), e2, t); t
          end
        | _ => error("CaseExp: sum type expected")
        )
    | elabExp(D, G, UnrollExp(e)) =
        (case elabExp(D, G, e) of  (*TODO: higher kinds *)
          t as RecTyp(a, k, t) => substTyp (VarMap.singleton(a, t)) t'
        | _ => error("UnrollExp: recursive type expected")
        )
    | elabExp(D, G, InstExp(e, t)) =
        (case elabExp(D, G, e) of
          UnivTyp(a, k, t') =>
            (elabTypAs(D, t, k); substTyp (VarMap.singleton(a, t)) t')
        | _ => error("InstExp: universal type expected")
        )
    | elabExp(D, G, UnpackExp(e1, a, x, e2)) =
        (case elabExp(D, G, e1) of
          ExistTyp(a', k, t) =>
          let
            val t' = substTyp (VarMap.singleton(a', VarTyp(a))) t
            val t2 = elabExp(VarMap.insert(D, a, k), VarMap.insert(G, x, t'))
          in
            elabTypAs(D, t2, StarKind); t2
          end
        | _ => error("UnpackExp: existential type expected")
        )
    | elabExp(D, G, TestExp(e1, e2, x, e3, e4)) =
      (
        elabExpAs(D, G, e1, ExnTyp);
        (case elabExp(D, G, e2) of
          TagTyp(t') =>
          let
            val t = elabExp(D, VarMap.insert(G, x, t'), e3)
          in
            elabExpAs(D, G, e4, t); t
          end
        | _ => error("TestExp: tag type expected")
        )
      )
    | elabExp(D, G, ThrowExp(e, as_t)) =
        (elabTypAs(D, as_t, StarKind); elabExpAs(D, G, e, ExnTyp); as_t)
    | elabExp(D, G, CatchExp(e1, x, e2)) =
      let
        val t = elabExp(D, G, e1)
      in
        elabExpAs(D, VarMap.insert(G, x, ExnTyp), e2, t); t
      end
    | elabExp(D, G, RefExp(e)) =
        RefTyp(elabExp(D, G, e))
    | elabExp(D, G, ReadExp(e)) =
        (case elabExp(D, G, e) of
          RefTyp(t) => t
        | _ => error("ReadExp: reference type expected")
        )
    | elabExp(D, G, WriteExp(e1, e2)) =
        (case elabExp(D, G, e1) of
          RefTyp(t) => (elabExpAs(D, G, e2, t); UnitExp)
        | _ => error("WriteExp: reference type expected")
        )
end;
