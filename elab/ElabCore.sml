(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core elaboration
 *
 * Definition, Sections 4.10, 4.11, 4.6, 4.7, 2.9
 *
 * Notes:
 * - See also ELAB_CORE-sig.sml
 * - To implement overloading resolution and checks for flexible records,
 *   we accumulate lists of unresolved types at each value declaration.
 *   This requires an additional argument to most elab functions.
 * - The Definition says that overloaded types get defaulted if the
 *   "surrounding text" does not resolve it. It leaves some freedom to
 *   how large this context may be. We choose the innermost value binding.
 * - The Definition states that "the program context" must determine the
 *   exact type of flexible records, but it does not say how large this
 *   context may be either. Again we choose the innermost surrounding value
 *   declaration.
 *)

structure ElabCore : ELAB_CORE =
struct
  (* Import *)

  open SyntaxCore
  open AnnotationCore
  open StaticObjectsCore
  open Error


  (* Helpers for context modification and side conditions *)

  val plus         = StaticEnv.plus
  val plusU        = Context.plusU
  val plusVE       = Context.plusVE
  val oplusE       = Context.oplusE
  val oplusTE      = Context.oplusTE
  val oplusVEandTE = Context.oplusVEandTE

  infix plusU plusVE oplusE oplusTE oplusVEandTE
  infix --> |->

  fun ?elab(C, NONE)        default = default
    | ?elab(C, SOME phrase) default = elab(C, phrase)

  exception Check

  fun check true  = ()
    | check false = raise Check


  (* Management of unresolved types [Sections 4.11 and Appendix E] *)

  datatype level  = TOP | INNER

  type unresolved = Source.loc * Type * SCon option
  type deferred =
      { level      : level,
        unresolved : unresolved list ref,
        matches    : (Env * Match) list ref
      }

  fun deferred level = {level = level, unresolved = ref [], matches = ref []}
  fun push(r, x)     = r := x :: !r

  fun instance (loc, D : deferred) sigma =
      let
        val (taus, tau) = TypeScheme.instance sigma
      in
        List.app
          (fn tau => push(#unresolved D, (loc, tau, NONE)))
          (List.filter Type.isOverloaded taus);
        tau
      end

  fun resolve(D : deferred) =
      List.app
        (fn(loc, tau, sc_opt) =>
          ( (* Further restriction [Section 4.11, item 1] *)
            Type.resolve tau handle Type.Flexible =>
              error(loc, "unresolved flexible record type");
            case sc_opt of
              NONE        => ()
            | SOME(sc@@A) =>
                let
                  val t = Type.tyname tau --> elab A
                in
                  (* [Section E.1] *)
                  (case sc of
                    SCon.INT(b, s) =>
                      ignore(LibrarySVal.intFromString(b, s, SOME t))
                  | SCon.WORD(b, s) =>
                      ignore(LibrarySVal.wordFromString(b, s, SOME t))
                  | SCon.CHAR(s) =>
                      ignore(LibrarySVal.charFromString(s, SOME t))
                  | SCon.STRING(s) =>
                      ignore(LibrarySVal.stringFromString(s, SOME t))
                  | SCon.REAL(s) =>
                      ignore(LibrarySVal.realFromString(s, SOME t))
                  ) handle Overflow =>
                    error(loc, "special constant out of range")
                end
          )
        ) (!(#unresolved D))


  (* Type variable sequences *)

  fun tyvars(Seq(tyvars)@@_) = List.map syntax tyvars


  (* Typing special constants [Section 4.1, Appendix E.1] *)

  fun typeSCon (D : deferred) (scon@@A) =
      let
        val oc =
            case scon of
              SCon.INT _    => StaticLibrary.Int
            | SCon.WORD _   => StaticLibrary.Word
            | SCon.CHAR _   => StaticLibrary.Char
            | SCon.STRING _ => StaticLibrary.String
            | SCon.REAL _   => StaticLibrary.Real
        val tau = Type.fromOverloadingClass oc
      in
        push(#unresolved D, (loc A, tau, SOME(scon@@A)));
        tau
      end


  (* Looking up identifiers *)

  fun findLongVId(C, longvid@@A) =
      (case Context.findLongVId(C, longvid) of
        SOME valstr => valstr
      | NONE => errorLongVId(loc A, "unknown identifier ", longvid)
      ) --> elab A

  fun findLongTyCon(C, longtycon@@A) =
      (case Context.findLongTyCon(C, longtycon) of
        SOME tystr => tystr
      | NONE => errorLongTyCon(loc A, "unknown type ", longtycon)
      ) --> elab A

  fun findLongStrId(C, longstrid@@A) =
      (case Context.findLongStrId(C, longstrid) of
        SOME E => E
      | NONE => errorLongStrId(loc A, "unknown structure ", longstrid)
      ) --> elab A


  (* Inference rules [Section 4.10] *)

  (* Atomic Expressions *)

  fun elabAtExp D (C, SCONAtExp(scon)@@A) =
      (* [Rule 1] *)
      let
      in
        typeSCon D scon
      end --> elab A
    | elabAtExp D (C, IDAtExp(_, longvid)@@A) =
      (* [Rule 2] *)
      let
        val (sigma, is) = findLongVId(C, longvid)
        val tau         = instance (loc A, D) sigma
      in
        tau
      end --> elab A
    | elabAtExp D (C, RECORDAtExp(exprow_opt)@@A) =
      (* [Rule 3] *)
      let
        val rho = ?(elabExpRow D) (C, exprow_opt) Type.emptyRow
      in
        Type.fromRowType rho
      end --> elab A
    | elabAtExp D (C, LETAtExp(dec, exp)@@A) =
      (* [Rule 4] *)
      let
        val E   = elabDec INNER (C, dec)
        val tau = elabExp D (C oplusE E, exp)
      in
        check(TyNameSet.isSubset(Type.tynames tau, Context.Tof C))
          handle Check =>
            error(loc A, "escaping local type name in let expression");
        tau
      end --> elab A
    | elabAtExp D (C, PARAtExp(exp)@@A) =
      (* [Rule 5] *)
      let
        val tau = elabExp D (C, exp)
      in
        tau
      end --> elab A


  (* Expression Rows *)

  and elabExpRow D (C, ExpRow(lab@@_, exp, exprow_opt)@@A) =
      (* [Rule 6] *)
      let
        val tau = elabExp D (C, exp)
        val rho = ?(elabExpRow D) (C, exprow_opt) Type.emptyRow
      in
        Type.insertRow(rho, lab, tau)
      end --> elab A


  (* Expressions *)

  and elabExp D (C, ATExp(atexp)@@A) =
      (* [Rule 7] *)
      let
        val tau = elabAtExp D (C, atexp)
      in
        tau
      end --> elab A
    | elabExp D (C, APPExp(exp, atexp)@@A) =
      (* [Rule 8] *)
      let
        val tau_exp = elabExp D (C, exp)
        val tau'    = elabAtExp D (C, atexp)
        val tau     = Type.guess false
      in
        Type.unify(tau_exp, Type.fromFunType(tau', tau)) handle Type.Unify =>
          error(loc A, "type mismatch on application");
        tau
      end --> elab A
    | elabExp D (C, COLONExp(exp, ty)@@A) =
      (* [Rule 9] *)
      let
        val tau    = elabExp D (C, exp)
        val tau_ty = elabTy(C, ty)
      in
        Type.unify(tau, tau_ty) handle Type.Unify =>
          error(loc A, "expression does not match annotation");
        tau
      end --> elab A
    | elabExp D (C, HANDLEExp(exp, match)@@A) =
      (* [Rule 10] *)
      let
        val tau       = elabExp D (C, exp)
        val tau_match = elabMatch D (C, match)
      in
        Type.unify(Type.fromFunType(InitialStaticEnv.tauExn, tau), tau_match)
          handle Type.Unify =>
            error(loc A, "type mismatch in handler");
        tau
      end --> elab A
    | elabExp D (C, RAISEExp(exp)@@A) =
      (* [Rule 11] *)
      let
        val tau_exp = elabExp D (C, exp)
        val tau     = Type.guess false
      in
        Type.unify(tau_exp, InitialStaticEnv.tauExn) handle Type.Unify =>
          error(loc A, "raised expression is not an exception");
        tau
      end --> elab A
    | elabExp D (C, FNExp(match)@@A) =
      (* [Rule 12] *)
      let
        val tau = elabMatch D (C, match)
      in
        (* Further restriction [Section 4.11, item 2] *)
        push(#matches D, (Context.Eof C, match));
        tau
      end --> elab A


  (* Matches *)

  and elabMatch D (C, Match(mrule, match_opt)@@A) =
      (* [Rule 13] *)
      let
        val tau       = elabMrule D (C, mrule)
        val tau_match = ?(elabMatch D) (C, match_opt) tau
      in
        Type.unify(tau, tau_match) handle Type.Unify =>
          error(loc A, "type mismatch between different matches");
        tau
      end --> elab A


  (* Match rules *)

  and elabMrule D (C, Mrule(pat, exp)@@A) =
      (* [Rule 14] *)
      let
        val (VE, tau) = elabPat D (C, pat)
        val tau'      = elabExp D (C plusVE VE, exp)
      in
        check(TyNameSet.isSubset(StaticEnv.tynamesVE VE, Context.Tof C))
          handle Check =>
            (* Side condition is always ensured by stamping. *)
            error(loc A, "inconsistent type names");
        Type.fromFunType(tau, tau')
      end --> elab A


  (* Declarations *)

  and elabDec level (C, VALDec(tyvarseq, valbind)@@A) =
      (* [Rule 15] *)
      let
        val alphas = tyvars(tyvarseq)
        (* Collect implicitly bound tyvars [Section 4.6] *)
        val U =
            TyVarSet.union(
              TyVarSet.fromList alphas,
              TyVarSet.difference(
                ScopeTyVars.unguardedTyVars valbind, Context.Uof C))
        val D   = deferred level
        val VE  = elabValBind D (C plusU U, valbind)
        val _   = resolve D
        val VE' = Clos.Clos (C, valbind) VE
      in
        (* Further restriction [Section 4.11, item 2] *)
        List.map
          (fn(E, match@@A') =>
            CheckPattern.checkMatch(E, match@@A') --> exhaustive A'
          ) (!(#matches D));
        check(TyVarSet.disjoint(U, StaticEnv.tyvarsVE VE'))
          handle Check =>
            error(loc A, "some explicit type variables cannot be generalised");
        StaticEnv.fromVE VE'
      end --> elab A
    | elabDec level (C, TYPEDec(typbind)@@A) =
      (* [Rule 16] *)
      let
        val TE = elabTypBind(C, typbind)
      in
        StaticEnv.fromTE TE
      end --> elab A
    | elabDec level (C, DATATYPEDec(datbind)@@A) =
      (* [Rule 17] *)
      let
        val TE_rec   = recDatBind(C, datbind)
        val (VE, TE) = elabDatBind(C oplusTE TE_rec, datbind)
      in
        StaticEnv.maximiseEquality TE;
        check(
          TyConMap.all
            (fn(t, VE') =>
              not(TyNameSet.member(Context.Tof C, TypeFcn.toTyName t))
            ) TE
        ) handle Check =>
          (* Side condition is always ensured by stamping. *)
          error(loc A, "inconsistent type names");
        StaticEnv.fromVEandTE(VE, TE)
      end --> elab A
    | elabDec level (C, DATATYPE2Dec(tycon, longtycon)@@A) =
      (* [Rule 18] *)
      let
        val (theta, VE) = findLongTyCon(C, longtycon)
        val TE          = TyConMap.singleton(tycon |-> (theta, VE))
      in
        StaticEnv.fromVEandTE(VE, TE)
      end --> elab A
    | elabDec level (C, ABSTYPEDec(datbind, dec)@@A) =
      (* [Rule 19] *)
      let
        val TE_rec   = recDatBind(C, datbind)
        val (VE, TE) = elabDatBind(C oplusTE TE_rec, datbind)
        val _        = StaticEnv.maximiseEquality TE
        val E        = elabDec INNER (C oplusVEandTE (VE, TE), dec)
      in
        check(
          TyConMap.all
            (fn(t, VE') =>
              not(TyNameSet.member(Context.Tof C, TypeFcn.toTyName t))
            ) TE
        ) handle Check =>
          (* Side condition is always ensured by stamping. *)
          error(loc A, "inconsistent type names");
        StaticEnv.Abs(TE, E)
      end --> elab A
    | elabDec level (C, EXCEPTIONDec(exbind)@@A) =
      (* [Rule 20] *)
      let
        val VE = elabExBind(C, exbind)
      in
        StaticEnv.fromVE VE
      end --> elab A
    | elabDec level (C, LOCALDec(dec1, dec2)@@A) =
      (* [Rule 21] *)
      let
        val E1 = elabDec INNER (C, dec1)
        val E2 = elabDec INNER (C oplusE E1, dec2)
      in
        E2
      end --> elab A
    | elabDec level (C, OPENDec(longstrids)@@A) =
      (* [Rule 22] *)
      let
        val Es =
            List.map (fn longstrid => findLongStrId(C, longstrid)) longstrids
      in
        List.foldr StaticEnv.plus StaticEnv.empty Es
      end --> elab A
    | elabDec level (C, EMPTYDec@@A) =
      (* [Rule 23] *)
      let
      in
        StaticEnv.empty
      end --> elab A
    | elabDec level (C, SEQDec(dec1, dec2)@@A) =
      (* [Rule 24] *)
      let
        val E1 = elabDec level (C, dec1)
        val E2 = elabDec level (C oplusE E1, dec2)
      in
        StaticEnv.plus(E1, E2)
      end --> elab A


  (* Value Bindings *)

  and elabValBind D (C, PLAINValBind(pat, exp, valbind_opt)@@A) =
      (* [Rule 25] *)
      let
        val (VE, tau) = elabPat D (C, pat)
        val tau_exp   = elabExp D (C, exp)
        val VE'       = ?(elabValBind D) (C, valbind_opt) VIdMap.empty
      in
        Type.unify(tau, tau_exp) handle Type.Unify =>
          error(loc A, "type mismatch between pattern and expression");
        (* Further restriction [Section 4.11, item 3] *)
        (if #level D = TOP then StaticObjectsCore.NonExhaustive else
          CheckPattern.checkPat(Context.Eof C, pat) --> exhaustive A
        );
        VIdMap.unionWith #2 (VE, VE')
      end --> elab A
    | elabValBind D (C, RECValBind(valbind)@@A) =
      (* [Rule 26] *)
      let
        val VE_rec = recValBind(C, valbind)
        val VE     = elabValBind D (C plusVE VE_rec, valbind)
      in
        check(StaticEnv.equalsVE(VE, VE_rec)) handle Check =>
          error(loc A, "type mismatch in recursive binding")
        check(TyNameSet.isSubset(StaticEnv.tynamesVE VE, Context.Tof C))
          handle Check =>
            (* Side condition is always ensured by construction. *)
            error(loc A, "invalid introduction of type names");
        VE
      end --> elab A


  (* Type Bindings *)

  and elabTypBind(C, TypBind(tyvarseq, tycon, ty, typbind_opt)@@A) =
      (* [Rule 27] *)
      let
        val alphas = tyvars(tyvarseq)
        val tau    = elabTy(C, ty)
        val TE     = ?elabTypBind(C, typbind_opt) TyConMap.empty
      in
        TyConMap.extend(TE, tycon |-> ((alphas, tau), VIdMap.empty))
      end --> elab A


  (* Datatype Bindings *)

  and elabDatBind(C, DatBind(tyvarseq, tycon, conbind, datbind_opt)@@A) =
      (* [Rule 28, part 2] *)
      let
        (* tycon has been inserted into C by recDatBind *)
        val ((alphas, tau), _) = valOf(Context.findTyCon(C, syntax tycon))
        val t  = TypeFcn.toTyName(alphas, tau)
        val VE = elabConBind((C, tau), conbind)
        val (VE', TE') =
            ?elabDatBind(C, datbind_opt) (VIdMap.empty, TyConMap.empty)
        val ClosVE = StaticEnv.Clos VE
      in
        check(TyName.arity t = List.length alphas) handle Check =>
          (* Always consistent by construction. *)
          error(loc A, "inconsistent type arity");
        check(
          TyConMap.all (fn(t', VE'') => t <> TypeFcn.toTyName t') TE'
        ) handle Check =>
          (* Side condition is always ensured by stamping. *)
          error(loc A, "inconsistent type names");
        ( VIdMap.unionWith #2 (ClosVE, VE'),
          TyConMap.extend(TE', tycon |-> ((alphas, tau), ClosVE))
        )
      end --> elab A


  (* Constructor Bindings *)

  and elabConBind((C, tau), ConBind(_, vid, ty_opt, conbind_opt)@@A) =
      (* [Rule 29] *)
      let
        val tau' = ?elabTy(C, ty_opt) tau
        val VE   = ?elabConBind((C, tau), conbind_opt) VIdMap.empty
        val tau_vid =
            if Option.isSome ty_opt then Type.fromFunType(tau', tau) else tau
      in
        VIdMap.extend(VE, vid |-> (([], tau_vid), IdStatus.c))
      end --> elab A


  (* Exception Bindings *)

  and elabExBind(C, NEWExBind(_, vid, ty_opt, exbind_opt)@@A) =
      (* [Rule 30] *)
      let
        val tau = ?elabTy(C, ty_opt) InitialStaticEnv.tauExn
        val VE  = ?elabExBind(C, exbind_opt) VIdMap.empty
        val tau_vid =
            if Option.isSome ty_opt then
              Type.fromFunType(tau, InitialStaticEnv.tauExn)
            else
              InitialStaticEnv.tauExn
      in
        VIdMap.extend(VE, vid |-> (([], tau_vid), IdStatus.e))
      end --> elab A
    | elabExBind(C, EQUALExBind(_, vid, _, longvid, exbind_opt)@@A) =
      (* [Rule 31] *)
      let
        val ((alphas, tau), is_longvid) = findLongVId(C, longvid)
        val VE = ?elabExBind(C, exbind_opt) VIdMap.empty
      in
        check(is_longvid = IdStatus.e andalso List.null alphas) handle Check =>
          errorLongVId(loc(annotation longvid),
            "non-exception identifier ", syntax longvid);
        VIdMap.extend(VE, vid |-> (([], tau), IdStatus.e))
      end --> elab A


  (* Atomic Patterns *)

  and elabAtPat D (C, WILDCARDAtPat@@A) =
      (* [Rule 32] *)
      let
        val tau = Type.guess false
      in
        (VIdMap.empty, tau)
      end --> elab A
    | elabAtPat D (C, SCONAtPat(scon)@@A) =
      (* [Rule 33] *)
      let
      in
        (VIdMap.empty, typeSCon D scon)
      end --> elab A
    | elabAtPat D (C, IDAtPat(_, longvid as longvid'@@A')@@A) =
      (* [Rules 34 and 35] *)
      let
        val (strids, vid) = LongVId.explode longvid'
      in
        if
          List.null strids andalso
          case Context.findVId(C, vid) of
            NONE            => true
          | SOME(sigma, is) => is = IdStatus.v
        then
          (* [Rule 34] *)
          let
            val tau = Type.guess false
          in
            (VIdMap.singleton(vid@@A' |-> (([], tau), IdStatus.v)), tau)
          end
        else
          (* [Rule 35] *)
          let
            val (sigma, is) = findLongVId(C, longvid)
            val _ = check(is <> IdStatus.v)
                handle Check =>
                  error(loc A', "non-constructor long identifier in pattern")
            val (taus, t) = Type.toConsType(instance (loc A, D) sigma)
                handle Type =>
                  error(loc A, "missing constructor argument in pattern")
          in
            (VIdMap.empty, Type.fromConsType(taus, t))
          end
      end --> elab A
    | elabAtPat D (C, RECORDAtPat(patrow_opt)@@A) =
      (* [Rule 36] *)
      let
        val (VE, rho) =
            ?(elabPatRow D) (C, patrow_opt) (VIdMap.empty, Type.emptyRow)
        val tau_rho = Type.fromRowType rho
      in
        push(#unresolved D, (loc A, tau_rho, NONE));
        (VE, tau_rho)
      end --> elab A
    | elabAtPat D (C, PARAtPat(pat)@@A) =
      (* [Rule 37] *)
      let
        val (VE, tau) = elabPat D (C, pat)
      in
        (VE, tau)
      end --> elab A


  (* Pattern Rows *)

  and elabPatRow D (C, DOTSPatRow@@A) =
      (* [Rule 38] *)
      let
        val rho = Type.guessRow()
      in
        (VIdMap.empty, rho)
      end --> elab A
    | elabPatRow D (C, FIELDPatRow(lab@@_, pat, patrow_opt)@@A) =
      (* [Rule 39] *)
      let
        val (VE, tau) = elabPat D (C, pat)
        val (VE', rho) =
            ?(elabPatRow D) (C, patrow_opt) (VIdMap.empty, Type.emptyRow)
      in
        ( VIdMap.unionWithi
            (fn(vid, _, _) => errorVId(loc A, "duplicate variable ", vid))
            (VE, VE'),
          Type.insertRow(rho, lab, tau)
        )
      end --> elab A


  (* Patterns *)

  and elabPat D (C, ATPat(atpat)@@A) =
      (* [Rule 40] *)
      let
        val (VE, tau) = elabAtPat D (C, atpat)
      in
        (VE, tau)
      end --> elab A
    | elabPat D (C, CONPat(_, longvid as longvid'@@A', atpat)@@A) =
      (* [Rule 41] *)
      let
        val (sigma, is) = findLongVId(C, longvid)
        val _ = check(is <> IdStatus.v)
            handle Check =>
              errorLongVId(loc A', "non-constructor ", longvid')
        val (tau', tau) = Type.toFunType(instance (loc A', D) sigma)
            handle Type =>
              errorLongVId(loc A', "misplaced nullary constructor ", longvid')
        val (VE, tau_atpat) = elabAtPat D (C, atpat)
      in
        Type.unify(tau', tau_atpat) handle Type.Unify =>
          error(loc A, "type mismatch in constructor pattern");
        (VE, tau)
      end --> elab A
    | elabPat D (C, COLONPat(pat, ty)@@A) =
      (* [Rule 42] *)
      let
        val (VE, tau) = elabPat D (C, pat)
        val tau_ty    = elabTy(C, ty)
      in
        Type.unify(tau, tau_ty) handle Type.Unify =>
          error(loc A, "pattern does not match annotation");
        (VE, tau)
      end --> elab A
    | elabPat D (C, ASPat(_, vid as vid'@@A', ty_opt, pat)@@A) =
      (* [Rule 43] *)
      let
        val _ =
            check(
              case Context.findVId(C, vid') of
                NONE            => true
              | SOME(sigma, is) => is = IdStatus.v
            ) handle Check =>
              error(loc A', "misplaced constructor in pattern")
        val (VE, tau) = elabPat D (C, pat)
        val tau_ty    = ?elabTy(C, ty_opt) tau
      in
        Type.unify(tau, tau_ty) handle Type.Unify =>
          error(loc A, "pattern does not match annotation");
        check(not(VIdMap.inDomain(VE, vid'))) handle Check =>
          errorVId(loc A', "duplicate variable ", vid');
        (VIdMap.extend(VE, vid |-> (([], tau), IdStatus.v)), tau)
      end --> elab A


  (* Type Expressions *)

  and elabTy(C, VARTy(tyvar)@@A) =
      (* [Rule 44] *)
      let
        val alpha@@_ = tyvar
      in
        Type.fromTyVar alpha
      end --> elab A
    | elabTy(C, RECORDTy(tyrow_opt)@@A) =
      (* [Rule 45] *)
      let
        val rho = ?elabTyRow(C, tyrow_opt) Type.emptyRow
      in
        Type.fromRowType rho
      end --> elab A
    | elabTy(C, CONTy(tyseq, longtycon)@@A) =
      (* [Rule 46] *)
      let
        val Seq(tys)@@_ = tyseq
        val taus        = List.map (fn ty => elabTy(C, ty)) tys
        val (theta, VE) = findLongTyCon(C, longtycon)
      in
        TypeFcn.apply(taus, theta) handle TypeFcn.Apply =>
          error(loc A, "arity mismatch in type application")
      end --> elab A
    | elabTy(C, ARROWTy(ty, ty')@@A) =
      (* [Rule 47] *)
      let
        val tau  = elabTy(C, ty)
        val tau' = elabTy(C, ty')
      in
        Type.fromFunType(tau, tau')
      end --> elab A
    | elabTy(C, PARTy(ty)@@A) =
      (* [Rule 48] *)
      let
        val tau = elabTy(C, ty)
      in
        tau
      end --> elab A


  (* Type-expression Rows *)

  and elabTyRow(C, TyRow(lab@@_, ty, tyrow_opt)@@A) =
      (* [Rule 49] *)
      let
        val tau = elabTy(C, ty)
        val rho = ?elabTyRow(C, tyrow_opt) Type.emptyRow
      in
        Type.insertRow(rho, lab, tau)
      end --> elab A


  (* Build tentative VE from LHSs of recursive valbind *)

  and recValBind(C, PLAINValBind(pat, exp, valbind_opt)@@A) =
      let
        val VE  = recPat(C, pat)
        val VE' = ?recValBind(C, valbind_opt) VIdMap.empty
      in
        VIdMap.unionWith #2 (VE, VE')
      end
    | recValBind(C, RECValBind(valbind)@@A) =
        recValBind(C, valbind)

  and recPat(C, ATPat(atpat)@@A) =
        recAtPat(C, atpat)
    | recPat(C, CONPat(_, longvid, atpat)@@A) =
        recAtPat(C, atpat)
    | recPat(C, COLONPat(pat, ty)@@A) =
        recPat(C, pat)
    | recPat(C, ASPat(_, vid@@_, ty_opt, pat)@@A) =
        VIdMap.insert(recPat(C, pat), vid, (([], Type.guess false), IdStatus.v))

  and recAtPat(C, WILDCARDAtPat@@A) =
        VIdMap.empty
    | recAtPat(C, SCONAtPat(scon)@@A) =
        VIdMap.empty
    | recAtPat(C, IDAtPat(_, longvid@@_)@@A) =
        (case LongVId.explode longvid of
          ([], vid) =>
            VIdMap.singleton(vid, (([], Type.guess false), IdStatus.v))
        | _ => VIdMap.empty
        )
    | recAtPat(C, RECORDAtPat(patrow_opt)@@A) =
        ?recPatRow(C, patrow_opt) VIdMap.empty
    | recAtPat(C, PARAtPat(pat)@@A) =
        recPat(C, pat)

  and recPatRow(C, DOTSPatRow@@A) =
        VIdMap.empty
    | recPatRow(C, FIELDPatRow(lab, pat, patrow_opt)@@A) =
      let
        val VE  = recPat(C, pat)
        val VE' = ?recPatRow(C, patrow_opt) VIdMap.empty
      in
        VIdMap.unionWith #2 (VE, VE')
      end


  (* Build tentative TE from LHSs of recursive datbind *)

  and recDatBind(C, DatBind(tyvarseq, tycon@@_, conbind, datbind_opt)@@A) =
      (* [Rule 28, part 1] *)
      let
        val alphas = tyvars(tyvarseq)
        val k      = List.length alphas
        val span   = recConBind(C, conbind)
        val t      = TyName.tyname(TyCon.toString tycon, k, true, span)
        val tau    = Type.fromConsType(List.map Type.fromTyVar alphas, t)
        val TE'    = ?recDatBind(C, datbind_opt) TyConMap.empty
      in
        TyConMap.insert(TE', tycon, ((alphas, tau), VIdMap.empty))
      end

  and recConBind(C, ConBind(_, vid, ty_opt, conbind_opt)@@A) =
      1 + ?recConBind(C, conbind_opt) 0
end;
