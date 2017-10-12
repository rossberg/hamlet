(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML modules elaboration
 *
 * Definition, Sections 5.7 and 3.5
 *
 * Notes:
 * - To implement the 3rd restriction in 4.11 some elab functions are
 *   passed an additional boolean argument to indicate the toplevel.
 * - Another bug in the Definition -- rules 64 and 78 need additional
 *   side conditions to ensure well-formedness of the constructed realisation:
 *   (64) t in TyName^(k)
 *   (78) t_i in TyName^(k), i = 1..n
 *)

structure ElabModule : ELAB_MODULE =
struct
  (* Import *)

  open SyntaxModule
  open AnnotationModule
  open StaticObjectsModule
  open StaticObjectsCore
  open Error


  (* Helpers for basis modification and side conditions *)

  val Cof     = StaticBasis.Cof
  val Tof     = StaticBasis.Tof
  val plus    = StaticBasis.plus
  val plusT   = StaticBasis.plusT
  val oplusSE = StaticBasis.oplusSE
  val oplusG  = StaticBasis.oplusG
  val oplusF  = StaticBasis.oplusF
  val oplusE  = StaticBasis.oplusE

  infix plus plusT oplusG oplusF oplusE oplusSE
  infix --> |->

  fun ?elab(B, NONE)        default = default
    | ?elab(B, SOME phrase) default = elab(B, phrase)

  exception Check

  fun check true  = ()
    | check false = raise Check


  (* Type variable sequences *)

  fun tyvars(SyntaxCore.Seq(tyvars)@@_) = List.map syntax tyvars


  (* Looking up identifiers *)

  fun findLongTyConE(E, longtycon@@A) =
      (case StaticEnv.findLongTyCon(E, longtycon) of
        SOME tystr => tystr
      | NONE => errorLongTyCon(loc A, "unknown type ", longtycon)
      ) --> elab A

  fun findLongStrIdE(E, longstrid@@A) =
      (case StaticEnv.findLongStrId(E, longstrid) of
        SOME E => E
      | NONE => errorLongStrId(loc A, "unknown structure ", longstrid)
      ) --> elab A

  fun findLongTyCon(B, longtycon) =
        findLongTyConE(Context.Eof(Cof B), longtycon)

  fun findLongStrId(B, longstrid) =
        findLongStrIdE(Context.Eof(Cof B), longstrid)

  fun findSigId(B, sigid@@A) =
      (case StaticBasis.findSigId(B, sigid) of
        SOME Sigma => Sigma
      | NONE => errorSigId(loc A, "unknown signature ", sigid)
      ) --> elab A

  fun findFunId(B, funid@@A) =
      (case StaticBasis.findFunId(B, funid) of
        SOME Phi => Phi
      | NONE => errorFunId(loc A, "unknown functor ", funid)
      ) --> elab A


  (* Inference rules [Section 5.7] *)

  (* Structure Expressions *)

  fun elabStrExp(B, STRUCTStrExp(strdec)@@A) =
      (* [Rule 50] *)
      let
        val E = elabStrDec ElabCore.INNER (B, strdec)
      in
        E
      end --> elab A
    | elabStrExp(B, IDStrExp(longstrid)@@A) =
      (* [Rule 51] *)
      let
        val E = findLongStrId(B, longstrid)
      in
        E
      end --> elab A
    | elabStrExp(B, COLONStrExp(strexp, sigexp)@@A) =
      (* [Rule 52] *)
      let
        val E       = elabStrExp(B, strexp)
        val Sigma   = elabSigExp(B, sigexp)
        val (E', _) = Sig.match(E, Sigma)
            handle Sig.Match =>
              error(loc A, "structure does not match annotation")
      in
        E'
      end --> elab A
    | elabStrExp(B, SEALStrExp(strexp, sigexp)@@A) =
      (* [Rule 53] *)
      let
        val E        = elabStrExp(B, strexp)
        val (T', E') = elabSigExp(B, sigexp)
        val (E'', _) = Sig.match(E, (T', E'))
            handle Sig.Match =>
              error(loc A, "structure does not match annotation")
      in
        check(TyNameSet.disjoint(T', Tof B)) handle Check =>
          (* Side condition is always ensured by renaming. *)
          error(loc A, "inconsistent type names");
        E'
      end --> elab A
    | elabStrExp(B, APPStrExp(funid, strexp)@@A) =
      (* [Rule 54] *)
      let
        val E = elabStrExp(B, strexp)
        val (T_funid, (E_funid, (T_funid', E_funid'))) = findFunId(B, funid)
        val (E'', phi) = Sig.match(E, (T_funid, E_funid))
            handle Sig.Match =>
              error(loc A, "signature mismatch in functor application")
        val (T', E') = Sig.rename(T_funid', StaticEnv.realise phi E_funid')
      in
        check(
          TyNameSet.disjoint(TyNameSet.union(StaticEnv.tynames E, Tof B), T')
        ) handle Check =>
          (* Side condition is always ensured by renaming. *)
          error(loc A, "inconsistent type names");
        E'
      end --> elab A
    | elabStrExp(B, LETStrExp(strdec, strexp)@@A) =
      (* [Rule 55] *)
      let
        val E1 = elabStrDec ElabCore.INNER (B, strdec)
        val E2 = elabStrExp(B oplusE E1, strexp)
      in
        E2
      end --> elab A


  (* Structure-level Declarations *)

  and elabStrDec level (B, DECStrDec(dec)@@A) =
      (* [Rule 56] *)
      let
        val E = ElabCore.elabDec level (Cof B, dec)
      in
        E
      end --> elab A
    | elabStrDec level (B, STRUCTUREStrDec(strbind)@@A) =
      (* [Rule 57] *)
      let
        val SE = elabStrBind(B, strbind)
      in
        StaticEnv.fromSE SE
      end --> elab A
    | elabStrDec level (B, LOCALStrDec(strdec1, strdec2)@@A) =
      (* [Rule 58] *)
      let
        val E1 = elabStrDec ElabCore.INNER (B, strdec1)
        val E2 = elabStrDec ElabCore.INNER (B oplusE E1, strdec2)
      in
        E2
      end --> elab A
    | elabStrDec level (B, EMPTYStrDec@@A) =
      (* [Rule 59] *)
      let
      in
        StaticEnv.empty
      end --> elab A
    | elabStrDec level (B, SEQStrDec(strdec1, strdec2)@@A) =
      (* [Rule 60] *)
      let
        val E1 = elabStrDec level (B, strdec1)
        val E2 = elabStrDec level (B oplusE E1, strdec2)
      in
        StaticEnv.plus(E1, E2)
      end --> elab A


  (* Structure Bindings *)

  and elabStrBind(B, StrBind(strid, strexp, strbind_opt)@@A) =
      (* [Rule 61] *)
      let
        val E  = elabStrExp(B, strexp)
        val SE =
            ?elabStrBind(B plusT StaticEnv.tynames E, strbind_opt)
              StrIdMap.empty
      in
        StrIdMap.extend(SE, strid |-> E)
      end --> elab A


  (* Signature Expressions *)

  and elabSigExpE(B, SIGSigExp(spec)@@A) =
      (* [Rule 62] *)
      let
        val E = elabSpec(B, spec)
      in
        E
      end --> elab A
    | elabSigExpE(B, IDSigExp(sigid)@@A) =
      (* [Rule 63] *)
      let
        val (T, E) = Sig.rename(findSigId(B, sigid))
      in
        check(TyNameSet.disjoint(T, Tof B)) handle Check =>
          (* Side condition is always ensured by renaming. *)
          error(loc A, "inconsistent type names");
        E
      end --> elab A
    | elabSigExpE(B,
        WHERETYPESigExp(sigexp, tyvarseq, longtycon as longtycon'@@A', ty)@@A
      ) =
      (* [Rule 64] *)
      let
        val E = elabSigExpE(B, sigexp)
        val alphas = tyvars(tyvarseq)
        val tau = ElabCore.elabTy(Cof B, ty)
        val t = TypeFcn.toTyName(#1(findLongTyConE(E, longtycon)))
            handle Type.Type =>
              errorLongTyCon(loc A', "non-flexible type ", longtycon')
        val _ = check(not(TyNameSet.member(Tof B, t)))
            handle Check =>
              errorLongTyCon(loc A', "rigid type ", longtycon')
        val phi = TyNameMap.singleton(t, (alphas, tau))
        val _ =
            check(
              not(TyName.admitsEquality t) orelse
              TypeFcn.admitsEquality(alphas, tau)
            ) handle Check =>
              error(loc A, "type realisation does not respect equality")
        val _ = check(TyName.arity t = List.length alphas)
            handle Check =>
              error(loc A, "type realisation does not respect arity")
        val phiE = StaticEnv.realise phi E
      in
        check(StaticEnv.isWellFormed phiE) handle Check =>
          error(loc A, "type realisation does not respect datatype");
        phiE
      end --> elab A

  and elabSigExp(B, sigexp) =
      (* [Rule 65] *)
      let
        val E = elabSigExpE(B, sigexp)
        val T = TyNameSet.difference(StaticEnv.tynames E, Tof B)
      in
        (T, E)
      end


  (* Signature Declarations *)

  and elabSigDec(B, SigDec(sigbind)@@A) =
      (* [Rule 66] *)
      let
        val G = elabSigBind(B, sigbind)
      in
        G
      end --> elab A


  (* Signature Bindings *)

  and elabSigBind(B, SigBind(sigid, sigexp, sigbind_opt)@@A) =
      (* [Rule 67] *)
      let
        val Sigma = elabSigExp(B, sigexp)
        val G     = ?elabSigBind(B, sigbind_opt) SigIdMap.empty
      in
        SigIdMap.extend(G, sigid |-> Sigma)
      end --> elab A


  (* Specifications *)

  and elabSpec(B, VALSpec(valdesc)@@A) =
      (* [Rule 68] *)
      let
        val VE = elabValDesc(Cof B, valdesc)
      in
        StaticEnv.fromVE(StaticEnv.Clos VE)
      end --> elab A
    | elabSpec(B, TYPESpec(typdesc)@@A) =
      (* [Rule 69] *)
      let
        val TE = elabTypDesc false (Cof B, typdesc)
      in
        check(
          TyConMap.all
            (fn(t, VE) => not(TyName.admitsEquality(TypeFcn.toTyName t))) TE
        ) handle Check =>
          (* Side condition is always ensured by elabTypDesc false. *)
          error(loc A, "inconsistent type names");
        StaticEnv.fromTE TE
      end --> elab A
    | elabSpec(B, EQTYPESpec(typdesc)@@A) =
      (* [Rule 70] *)
      let
        val TE = elabTypDesc true (Cof B, typdesc)
      in
        check(
          TyConMap.all
            (fn(t, VE) => TyName.admitsEquality(TypeFcn.toTyName t)) TE
        ) handle Check =>
          (* Side condition is always ensured by elabTypDesc true. *)
          error(loc A, "inconsistent type names");
        StaticEnv.fromTE TE
      end --> elab A
    | elabSpec(B, DATATYPESpec(datdesc)@@A) =
      (* [Rule 71] *)
      let
        val TE_rec   = recDatDesc(B, datdesc)
        val (VE, TE) = elabDatDesc(Context.oplusTE(Cof B, TE_rec), datdesc)
      in
        StaticEnv.maximiseEquality TE;
        check(
          TyConMap.all
            (fn(t, VE') =>
              not(TyNameSet.member(StaticBasis.tynames B, TypeFcn.toTyName t))
            ) TE
         ) handle Check =>
           (* Side condition is always ensured by stamping. *)
           error(loc A, "inconsistent type names");
        StaticEnv.fromVEandTE(VE, TE)
      end --> elab A
    | elabSpec(B, DATATYPE2Spec(tycon, longtycon)@@A) =
      (* [Rule 72] *)
      let
        val (theta, VE) = findLongTyCon(B, longtycon)
        val TE          = TyConMap.singleton(tycon |-> (theta, VE))
      in
        StaticEnv.fromVEandTE(VE, TE)
      end --> elab A
    | elabSpec(B, EXCEPTIONSpec(exdesc)@@A) =
      (* [Rule 73] *)
      let
        val VE = elabExDesc(Cof B, exdesc)
      in
        StaticEnv.fromVE VE
      end --> elab A
    | elabSpec(B, STRUCTURESpec(strdesc)@@A) =
      (* [Rule 74] *)
      let
        val SE = elabStrDesc(B, strdesc)
      in
        StaticEnv.fromSE SE
      end --> elab A
    | elabSpec(B, INCLUDESpec(sigexp)@@A) =
      (* [Rule 75] *)
      let
        val E = elabSigExpE(B, sigexp)
      in
        E
      end --> elab A
    | elabSpec(B, EMPTYSpec@@A) =
      (* [Rule 76] *)
      let
      in
        StaticEnv.empty
      end --> elab A
    | elabSpec(B, SEQSpec(spec1, spec2)@@A) =
      (* [Rule 77] *)
      let
        val E1 = elabSpec(B, spec1)
        val E2 = elabSpec(B oplusE E1, spec2)
      in
        check(StaticEnv.disjoint(E1, E2)) handle Check =>
          error(loc A, "duplicate specifications in signature");
        StaticEnv.plus(E1, E2)
      end --> elab A
    | elabSpec(B, SHARINGTYPESpec(spec, longtycons)@@A) =
      (* [Rule 78] *)
      let
        val E  = elabSpec(B, spec)
        val ts =
            List.map
              (fn longtycon as longtycon'@@A' =>
                let
                  val t = TypeFcn.toTyName(#1(findLongTyConE(E, longtycon)))
                in
                  if TyNameSet.member(Tof B, t) then
                    errorLongTyCon(loc A', "rigid type ", longtycon')
                  else
                    t
                end handle Type.Type =>
                  errorLongTyCon(loc A', "non-flexible type ", longtycon')
              ) longtycons
        val t =
            valOf(List.find
              (fn ti =>
                TyName.admitsEquality ti = List.exists TyName.admitsEquality ts
              ) ts)
        val _ =
            (* Implicit in definition of realisation, needs checking *)
            check(List.all (fn ti => TyName.arity ti = TyName.arity t) ts)
              handle Check =>
                error(loc A, "type sharing does not respect arity")
        val phi =
            List.foldl
              (fn(ti, phi) => TyNameMap.insert(phi, ti, TypeFcn.fromTyName t))
              TyNameMap.empty ts
      in
        TyName.adjustSpan(t, List.foldl Int.max 0 (List.map TyName.span ts));
        check(TyNameSet.disjoint(TyNameSet.fromList ts, Tof B)) handle Check =>
          (* Side condition is always ensured by stamping. *)
          error(loc A, "inconsistent type names");
        StaticEnv.realise phi E
      end --> elab A
    | elabSpec(B, SHARINGSpec(spec, longstrids)@@A) =
      (* [Appendix A] *)
      let
        fun shareFlexibleTyName(t1, t2, phi) =
            let
              (* Implicit in definition of realisation, but needs checking *)
              val _ = check(TyName.arity t1 = TyName.arity t2) handle Check =>
                  error(loc A, "sharing does not respect arity of type " ^
                    TyName.toString t1)
              val t =
                  TyName.tyname(
                    TyName.toString t1, TyName.arity t1,
                    TyName.admitsEquality t1 orelse TyName.admitsEquality t2,
                    Int.max(TyName.span t1, TyName.span t2))
              val theta = TypeFcn.fromTyName t
            in
              TyNameMap.insert(TyNameMap.insert(phi, t1, theta), t2, theta)
            end

        fun shareTE(TE1, TE2, phi) =
            TyConMap.foldli
              (fn(tycon, (theta1, VE1), phi) =>
                case TyConMap.find(TE2, tycon) of
                  NONE              => phi
                | SOME(theta2, VE2) =>
                  let
                    val t1 = TypeFcn.toTyName(TypeFcn.realise phi theta1)
                    val t2 = TypeFcn.toTyName(TypeFcn.realise phi theta2)
                  in
                    check(
                      not(TyNameSet.member(Tof B, t1)) andalso
                      not(TyNameSet.member(Tof B, t2))
                    ) handle Check =>
                      errorTyCon(loc A, "structure contains rigid type ",
                        tycon);
                    shareFlexibleTyName(t1, t2, phi)
                  end handle Type.Type =>
                    errorTyCon(loc A, "structure contains non-flexible type ", 
                      tycon)
              ) phi TE1

        fun shareSE(SE1, SE2, phi) =
            StrIdMap.foldli
              (fn(strid, E1, phi) =>
                case StrIdMap.find(SE2, strid) of
                  NONE    => phi
                | SOME E2 => shareE(E1, E2, phi)
              ) phi SE1

        and shareE(Env(SE1, TE1, VE1), Env(SE2, TE2, VE2), phi) =
            shareSE(SE1, SE2, shareTE(TE1, TE2, phi))

        fun shareOne(E1, [], phi)     = phi
          | shareOne(E1, E2::Es, phi) = shareOne(E1, Es, shareE(E1, E2, phi))

        fun shareAll([], phi)    = phi
          | shareAll(E::Es, phi) = shareAll(Es, shareOne(E, Es, phi))

        val E  = elabSpec(B, spec)
        val Es =
            List.map (fn longstrid => findLongStrIdE(E, longstrid)) longstrids
        val phi = shareAll(Es, TyNameMap.empty)
      in
        check(
          TyNameSet.disjoint(TyNameSet.fromList(TyNameMap.listKeys phi), Tof B)
        ) handle Check =>
          (* Side condition is always ensured by stamping. *)
          error(loc A, "inconsistent type names");
        StaticEnv.realise phi E
      end --> elab A


  (* Value Descriptions *)

  and elabValDesc(C, ValDesc(vid, ty, valdesc_opt)@@A) =
      (* [Rule 79] *)
      let
        val tau = ElabCore.elabTy(C, ty)
        val VE  = ?elabValDesc(C, valdesc_opt) VIdMap.empty
      in
        VIdMap.extend(VE, vid |-> (([], tau), IdStatus.v))
      end --> elab A


  (* Type Descriptions *)

  and elabTypDesc eq (C, TypDesc(tyvarseq, tycon, typdesc_opt)@@A) =
      (* [Rule 80] *)
      let
        val alphas = tyvars(tyvarseq)
        val k      = List.length alphas
        val t      = TyName.tyname(TyCon.toString(syntax tycon), k, eq, 0)
        val TE     = ?(elabTypDesc eq) (C, typdesc_opt) TyConMap.empty
        val tau    = Type.fromConsType (List.map Type.fromTyVar alphas, t)
      in
        check(not(TyNameSet.member(StaticEnv.tynamesTE TE, t))) handle Check =>
          (* Side condition is always ensured by stamping. *)
          error(loc A, "inconsistent type names");
        TyConMap.extend(TE, tycon |-> ((alphas, tau), VIdMap.empty))
      end --> elab A


  (* Datatype Descriptions *)

  and elabDatDesc(C, DatDesc(tyvarseq, tycon, condesc, datdesc_opt)@@A) =
      (* [Rule 81, part 2] *)
      let
        (* tycon has been inserted into C by recDatDesc *)
        val ((alphas, tau), _) = valOf(Context.findTyCon(C, syntax tycon))
        val t  = TypeFcn.toTyName(alphas, tau)
        val VE = elabConDesc((C, tau), condesc)
        val (VE', TE') =
            ?elabDatDesc(C, datdesc_opt) (VIdMap.empty, TyConMap.empty)
        val ClosVE   = StaticEnv.Clos VE
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


  (* Constructor Descriptions *)

  and elabConDesc((C, tau), ConDesc(vid, ty_opt, condesc_opt)@@A) =
      (* [Rule 82] *)
      let
        val tau' = ?ElabCore.elabTy(C, ty_opt) tau
        val VE   = ?elabConDesc((C, tau), condesc_opt) VIdMap.empty
        val tau_vid =
            if Option.isSome ty_opt then Type.fromFunType(tau', tau) else tau
      in
        VIdMap.extend(VE, vid |-> (([], tau_vid), IdStatus.c))
      end --> elab A


  (* Exception Description *)

  and elabExDesc(C, ExDesc(vid, ty_opt, exdesc_opt)@@A) =
      (* [Rule 83] *)
      let
        val tau = ?ElabCore.elabTy(C, ty_opt) InitialStaticEnv.tauExn
        val VE  = ?elabExDesc(C, exdesc_opt) VIdMap.empty
        val tau_vid =
            if Option.isSome ty_opt then
              Type.fromFunType(tau, InitialStaticEnv.tauExn)
            else
              InitialStaticEnv.tauExn
      in
        check(TyVarSet.isEmpty(Type.tyvars tau)) handle Check =>
          error(loc A, "free type variables in exception description");
        VIdMap.extend(VE, vid |-> (([], tau_vid), IdStatus.e))
      end --> elab A


  (* Structure Descriptions *)

  and elabStrDesc(B, StrDesc(strid, sigexp, strdesc_opt)@@A) =
      (* [Rule 84] *)
      let
        val E  = elabSigExpE(B, sigexp)
        val SE = ?elabStrDesc(B, strdesc_opt) StrIdMap.empty
      in
        StrIdMap.extend(SE, strid |-> E)
      end --> elab A


  (* Functor Declarations *)

  and elabFunDec(B, FunDec(funbind)@@A) =
      (* [Rule 85] *)
      let
        val F = elabFunBind(B, funbind)
      in
        F
      end --> elab A


  (* Functor Bindings *)

  and elabFunBind(B, FunBind(funid, strid, sigexp, strexp, funbind_opt)@@A) =
      (* [Rule 86] *)
      let
        val (T, E) = elabSigExp(B, sigexp)
        val E' = elabStrExp(B oplusSE StrIdMap.singleton(strid |-> E), strexp)
        val T' =
            TyNameSet.difference(
              StaticEnv.tynames E', TyNameSet.union(Tof B, T))
        val F  = ?elabFunBind(B, funbind_opt) FunIdMap.empty
      in
        check(TyNameSet.disjoint(T, Tof B)) handle Check =>
          (* Side condition is always ensured by stamping. *)
          error(loc A, "inconsistent type names");
        FunIdMap.extend(F, funid |-> (T, (E, (T', E'))))
      end --> elab A


  (* Top-level Declarations *)

  and elabTopDec(B, STRDECTopDec(strdec, topdec_opt)@@A) =
      (* [Rule 87] *)
      let
        val E   = elabStrDec ElabCore.TOP (B, strdec)
        val B'  = ?elabTopDec(B oplusE E, topdec_opt) StaticBasis.empty
        val B'' = StaticBasis.fromTandE(StaticEnv.tynames E, E) plus B'
      in
        check(TyVarSet.isEmpty(StaticBasis.tyvars B'')) handle Check =>
          error(loc A, "free type variables on top-level");
        (* Not stated in the Definition, but intended: *)
        check(StampMap.isEmpty(StaticBasis.undetermined B'')) handle Check =>
          error(loc A, "undetermined types on top-level");
        B''
      end --> elab A
    | elabTopDec(B, SIGDECTopDec(sigdec, topdec_opt)@@A) =
      (* [Rule 88] *)
      let
        val G   = elabSigDec(B, sigdec)
        val B'  = ?elabTopDec(B oplusG G, topdec_opt) StaticBasis.empty
        val B'' = StaticBasis.fromTandG(StaticBasis.tynamesG G, G) plus B'
      in
        B''
      end --> elab A
    | elabTopDec(B, FUNDECTopDec(fundec, topdec_opt)@@A) =
      (* [Rule 89] *)
      let
        val F   = elabFunDec(B, fundec)
        val B'  = ?elabTopDec(B oplusF F, topdec_opt) StaticBasis.empty
        val B'' = StaticBasis.fromTandF(StaticBasis.tynamesF F, F) plus B'
      in
        check(TyVarSet.isEmpty(StaticBasis.tyvars B'')) handle Check =>
          error(loc A, "free type variables on top-level");
        (* Not stated in the Definition, but intended: *)
        check(StampMap.isEmpty(StaticBasis.undetermined B'')) handle Check =>
          error(loc A, "undetermined types on top-level");
        B''
      end --> elab A


  (* Build tentative TE from LHSs of recursivedatdesc *)

  and recDatDesc(C, DatDesc(tyvarseq, tycon@@_, condesc, datdesc_opt)@@A) =
      (* [Rule 81, part 1] *)
      let
        val alphas = tyvars(tyvarseq)
        val k      = List.length alphas
        val span   = recConDesc(C, condesc)
        val t      = TyName.tyname(TyCon.toString tycon, k, true, span)
        val tau    = Type.fromConsType(List.map Type.fromTyVar alphas, t)
        val TE'    = ?recDatDesc(C, datdesc_opt) TyConMap.empty
      in
        TyConMap.insert(TE', tycon, ((alphas, tau), VIdMap.empty))
      end

  and recConDesc(C, ConDesc(vid, ty_opt, condesc_opt)@@A) =
      1 + ?recConDesc(C, condesc_opt) 0
end;
