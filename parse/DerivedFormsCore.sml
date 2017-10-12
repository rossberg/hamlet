(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core derived forms
 *
 * Definition, Section 2.7 and Appendix A
 *
 * Notes:
 * - See DERIVED_FORMS_CORE-sig.sml
 * - The Definition is somewhat inaccurate about the derived forms of Exp
 *   [Definition, Appendix A, Figure 15] in that most forms are actually AtExp
 *   derived forms, as can be seen from the full grammar [Definition,
 *   Appendix B, Figure 20]. To achieve consistency, the equivalent forms must
 *   be put in parentheses in some cases.
 * - The same goes for pattern derived forms [Definition, Appendix A, Figure 16;
 *   Appendix B, Figure 22].
 *)

structure DerivedFormsCore :> DERIVED_FORMS_CORE =
struct
  (* Import *)

  open SyntaxCore
  open AnnotationCore


  (* Types *)

  type AppExp    = AtExp list
  type InfExp    = Exp

  type FvalBind' = ValBind'
  type Fmatch'   = Match * VId * int
  type Fmrule'   = Mrule * VId * int
  type FvalBind  = ValBind
  type Fmatch    = (Fmatch', unit) phrase
  type Fmrule    = (Fmrule', unit) phrase


  (* Some helpers *)

  val vidFALSE        = VId.fromString "false"
  val vidTRUE         = VId.fromString "true"
  val vidNIL          = VId.fromString "nil"
  val vidCONS         = VId.fromString "::"

  fun atExp(atexp)    = ATExp(atexp)@@at(atexp)
  fun atPat(atpat)    = ATPat(atpat)@@at(atpat)
  fun idAtExp(vid@@A) = IDAtExp(NONE, LongVId.fromId(vid)@@A)@@at(vid@@A)
  fun idAtPat(vid@@A) = IDAtPat(NONE, LongVId.fromId(vid)@@A)@@at(vid@@A)
  fun idExp(vid)      = atExp(idAtExp(vid))
  fun idPat(vid)      = atPat(idAtPat(vid))


  (* Rewriting of withtype declarations [Appendix A, 2nd bullet] *)

  fun findTyCon tycon (TypBind(tyvarseq, tycon'@@_, ty, typbind_opt)@@_) =
      if tycon' = tycon then
        SOME(tyvarseq, ty)
      else
        Option.mapPartial (findTyCon tycon) typbind_opt

  fun copyTy(VARTy(tyvar@@A')@@A) =
        VARTy(tyvar@@copy(A'))@@copy(A)
    | copyTy(RECORDTy(tyrow_opt)@@A) =
        RECORDTy(Option.map copyTyRow tyrow_opt)@@copy(A)
    | copyTy(CONTy(tyseq, tycon@@A')@@A) =
        CONTy(copyTyseq tyseq, tycon@@copy(A'))@@copy(A)
    | copyTy(ARROWTy(ty1, ty2)@@A) =
        ARROWTy(copyTy ty1, copyTy ty2)@@copy(A)
    | copyTy(PARTy(ty)@@A) =
        PARTy(copyTy ty)@@copy(A)

  and copyTyRow(TyRow(lab@@A', ty, tyrow_opt)@@A) =
        TyRow(lab@@copy(A'), copyTy ty, Option.map copyTyRow tyrow_opt)@@copy(A)

  and copyTyseq(Seq(tys)@@A) =
        Seq(List.map copyTy tys)@@copy(A)

  fun replaceTy (Seq(tyvars)@@_, Seq(tys)@@A') (VARTy(tyvar@@_)@@A)=
      let
        fun loop((tyvar'@@_)::tyvars', ty'::tys') =
              if tyvar' = tyvar then copyTy ty' else loop(tyvars', tys')
          | loop([],_) = Error.error(loc A, "unbound type variable")
          | loop(_,[]) = Error.error(loc A', "type sequence has wrong arity")
      in
        loop(tyvars, tys)
      end
    | replaceTy tyvarseq_tyseq (RECORDTy(tyrow_opt)@@A) =
        RECORDTy(Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)@@copy(A)
    | replaceTy tyvarseq_tyseq (CONTy(tyseq', tycon@@A')@@A) =
        CONTy(replaceTyseq tyvarseq_tyseq tyseq', tycon@@copy(A'))@@copy(A)
    | replaceTy tyvarseq_tyseq (ARROWTy(ty1, ty2)@@A) =
        ARROWTy(replaceTy tyvarseq_tyseq ty1, replaceTy tyvarseq_tyseq ty2)
          @@copy(A)
    | replaceTy tyvarseq_tyseq (PARTy(ty)@@A) =
        PARTy(replaceTy tyvarseq_tyseq ty)@@copy(A)

  and replaceTyRow tyvarseq_tyseq (TyRow(lab@@A', ty, tyrow_opt)@@A) =
        TyRow(lab@@copy(A'), replaceTy tyvarseq_tyseq ty, 
          Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)@@copy(A)

  and replaceTyseq tyvarseq_tyseq (Seq(tys)@@A) =
        Seq(List.map (replaceTy tyvarseq_tyseq) tys)@@copy(A)


  fun rewriteTy typbind (VARTy(tyvar)@@A) =
        VARTy(tyvar)@@A
    | rewriteTy typbind (RECORDTy(tyrow_opt)@@A) =
        RECORDTy(Option.map (rewriteTyRow typbind) tyrow_opt)@@A
    | rewriteTy typbind (CONTy(tyseq, longtycon@@A')@@A) =
      let
        val tyseq' = rewriteTyseq typbind tyseq
        val (strids, tycon) = LongTyCon.explode longtycon
      in
        if not(List.null strids) then
          CONTy(tyseq', longtycon@@A')@@A
        else
          case findTyCon tycon typbind of
            SOME(tyvarseq', ty') => replaceTy (tyvarseq', tyseq') ty'
          | NONE                 => CONTy(tyseq', longtycon@@A')@@A
      end
    | rewriteTy typbind (ARROWTy(ty1, ty2)@@A) =
        ARROWTy(rewriteTy typbind ty1, rewriteTy typbind ty2)@@A
    | rewriteTy typbind (PARTy(ty)@@A) =
        PARTy(rewriteTy typbind ty)@@A

  and rewriteTyRow typbind (TyRow(lab, ty, tyrow_opt)@@A) =
        TyRow(lab, rewriteTy typbind ty,
          Option.map (rewriteTyRow typbind) tyrow_opt)@@A

  and rewriteTyseq typbind (Seq(tys)@@A) =
        Seq(List.map (rewriteTy typbind) tys)@@A

  fun rewriteConBind typbind (ConBind(op_opt, vid, ty_opt, conbind_opt)@@A)=
        ConBind(op_opt, vid,
          Option.map (rewriteTy typbind) ty_opt,
          Option.map (rewriteConBind typbind) conbind_opt
        )@@A

  fun rewriteDatBind typbind (
        DatBind(tyvarseq, tycon@@A', conbind, datbind_opt)@@A
      ) =
        case findTyCon tycon typbind of
          NONE =>
            DatBind(tyvarseq, tycon@@A', rewriteConBind typbind conbind,
              Option.map (rewriteDatBind typbind) datbind_opt)@@A
        | SOME _ =>
            Error.error(loc A,
              "duplicate type constructor in recursive type declaration")


  (* Patterns [Figure 16] *)

  val UNITAtPat = RECORDAtPat(NONE)

  fun TUPLEAtPat[pat] = PARAtPat(pat)
    | TUPLEAtPat(pats)  =
      let
        fun toPatRowOpt(n, []) = NONE
          | toPatRowOpt(n, pat::pats') =
            let
              val patrow_opt' = toPatRowOpt(n + 1, pats')
            in
              SOME(FIELDPatRow(Lab.fromInt(n)@@left(pat), pat, patrow_opt')
                @@overSome(pat, patrow_opt'))
            end
      in
        RECORDAtPat(toPatRowOpt(1, pats))
      end

  fun LISTAtPat[] = IDAtPat(NONE, LongVId.fromId(vidNIL)@@nowhere())
    | LISTAtPat(pats) =
      let
        val last = List.hd(List.rev pats)
        fun toPatList[] = idPat(vidNIL@@right(last))
          | toPatList(pat::pats') =
            let
              val patList' = toPatList pats'
              val tupPat = TUPLEAtPat[pat, patList']@@over(pat, last)
            in
              CONPat(NONE, LongVId.fromId(vidCONS)@@left(pat), tupPat)
                @@at(tupPat)
            end
      in
        PARAtPat(toPatList pats)
      end


  (* Pattern Rows [Figure 16] *)

  fun IDPatRow(vid as vid'@@_, ty_opt, pat_opt, patrow_opt) =
      let
        val lab = Lab.fromString(VId.toString vid')@@at(vid)
        val pat =
            case (ty_opt, pat_opt) of
              (NONE,    NONE) => idPat(vid)
            | (SOME ty, NONE) => COLONPat(idPat(vid), ty)@@over(vid, ty)
            | ( _ , SOME pat) => ASPat(NONE, vid, ty_opt, pat)@@over(vid, pat)
      in
        FIELDPatRow(lab, pat, patrow_opt)
      end


  (* Expressions [Figure 15] *)

  val UNITAtExp = RECORDAtExp(NONE)

  fun TUPLEAtExp[exp]  = PARAtExp(exp)
    | TUPLEAtExp(exps) =
      let
        fun toExpRowOpt(n, []) = NONE
          | toExpRowOpt(n, exp::exps') =
            let
              val exprow_opt' = toExpRowOpt(n + 1, exps')
            in
              SOME(ExpRow(Lab.fromInt(n)@@left(exp), exp, exprow_opt')
                @@overSome(exp, exprow_opt'))
            end
      in
        RECORDAtExp(toExpRowOpt(1, exps))
      end

  fun HASHAtExp(lab) =
      let
        val vid    = VId.invent()
        val vidPat = idPat(vid@@at(lab))
        val dots   = DOTSPatRow@@at(lab)
        val patrow = FIELDPatRow(lab, vidPat, SOME dots)@@at(lab)
        val pat    = atPat(RECORDAtPat(SOME patrow)@@at(lab))
        val mrule  = Mrule(pat, idExp(vid@@at(lab)))@@at(lab)
        val match  = Match(mrule, NONE)@@at(lab)
      in
        PARAtExp(FNExp(match)@@at(lab))
      end

  fun CASEExp(exp, match) =
      let
        val fnAtExp  = PARAtExp(FNExp(match)@@at(match))@@at(match)
        val argAtExp = PARAtExp(exp)@@at(exp)
      in
        APPExp(atExp(fnAtExp), argAtExp)
      end

  fun IFExp(exp1, exp2, exp3) =
      let
        val truePat    = idPat(vidTRUE@@left(exp2))
        val falsePat   = idPat(vidFALSE@@left(exp3))
        val trueMrule  = Mrule(truePat, exp2)@@at(exp2)
        val falseMrule = Mrule(falsePat, exp3)@@at(exp3)
        val elseMatch  = Match(falseMrule, NONE)@@at(exp3)
        val match      = Match(trueMrule, SOME elseMatch)@@over(exp2, exp3)
      in
        CASEExp(exp1, match)
      end

  fun ORELSEExp (exp1, exp2) =
        IFExp(exp1, idExp(vidTRUE@@left(exp2)), exp2)

  fun ANDALSOExp(exp1, exp2) =
        IFExp(exp1, exp2, idExp(vidFALSE@@right(exp2)))

  fun SEQAtExp(exps) =
      let
        fun toExpSeq[] = raise Fail "DerivedFormsCore.SEQAtExp: empty exp list"
          | toExpSeq[exp] = exp
          | toExpSeq(exp::exps') =
            let
              val pat   = atPat(WILDCARDAtPat@@right(exp))
              val mrule = Mrule(pat, toExpSeq exps')@@right(exp)
              val match = Match(mrule, NONE)@@right(exp)
            in
              CASEExp(exp, match)@@over(exp, match)
            end
      in
        PARAtExp(toExpSeq exps)
      end

  fun LETSEQAtExp(dec, [exp]) =
        LETAtExp(dec, exp)
    | LETSEQAtExp(dec,  exps) =
        LETAtExp(dec, atExp(SEQAtExp(exps)@@overAll(exps)))

  fun WHILEExp(exp1, exp2) =
      let
        val vid         = VId.invent()
        fun vidExp()    = idExp(vid@@right(exp2))
        fun unitAtExp() = UNITAtExp@@right(exp2)
        fun callExp()   = APPExp(vidExp(), unitAtExp())@@right(exp2)

        val seqExp      = atExp(SEQAtExp[exp2, callExp()]@@over(exp1, exp2))
        val unitExp     = atExp(unitAtExp())
        val ifExp       = IFExp(exp1, seqExp, unitExp)@@over(exp1, exp2)
        val unitPat     = atPat(UNITAtPat@@left(ifExp))
        val mrule       = Mrule(unitPat, ifExp)@@at(ifExp)
        val match       = Match(mrule, NONE)@@at(mrule)
        val fnExp       = FNExp(match)@@at(match)
        val vidPat      = idPat(vid@@left(exp1))
        val valbind     = PLAINValBind(vidPat, fnExp, NONE)@@at(fnExp)
        val recbind     = RECValBind(valbind)@@at(valbind)
        val tyvarseq    = Seq[]@@left(recbind)
        val dec         = VALDec(tyvarseq, recbind)@@at(recbind)
      in
        ATExp(LETAtExp(dec, callExp())@@at(dec))
      end

  fun LISTAtExp[] = IDAtExp(NONE, LongVId.fromId(vidNIL)@@nowhere())
    | LISTAtExp(exps) =
      let
        val last = List.hd(List.rev exps)
        fun toExpList[] = idExp(vidNIL@@right(last))
          | toExpList(exp::exps') =
            let
              val expList' = toExpList exps'
              val tupExp   = TUPLEAtExp[exp, expList']@@over(exp, last)
            in
              APPExp(idExp(vidCONS@@left(exp)), tupExp)@@at(tupExp)
            end
      in
        PARAtExp(toExpList exps)
      end


  (* Type Expressions [Figure 16] *)

  fun TUPLETy[ty'@@_] = ty'
    | TUPLETy(tys) =
      let
        fun toTyRowOpt(n,   []    ) = NONE
          | toTyRowOpt(n, ty::tys') =
            let
              val tyrow_opt' = toTyRowOpt(n+1, tys')
            in
              SOME(TyRow(Lab.fromInt(n)@@left(ty), ty, tyrow_opt')
                @@overSome(ty, tyrow_opt'))
            end
      in
        RECORDTy(toTyRowOpt(1, tys))
      end


  (* Function-value Bindings [Figure 17] *)

  fun FvalBind((match, vid, arity)@@A, fvalbind_opt) =
      let
        fun abstract(0, vidExps) =
            let
              val tupAtExp = TUPLEAtExp(List.rev vidExps)@@left(match)
            in
              CASEExp(atExp(tupAtExp), match)@@at(match)
            end
          | abstract(n, vidExps) =
            let
              val vid   = VId.invent()
              val exp   = idExp(vid@@left(match))
              val pat   = idPat(vid@@left(match))
              val body  = abstract(n-1, exp::vidExps)
              val mrule = Mrule(pat, body)@@at(pat)
            in
              FNExp(Match(mrule, NONE)@@at(mrule))@@at(mrule)
            end
      in
        PLAINValBind(idPat(vid), abstract(arity, []), fvalbind_opt)
      end

  fun Fmatch((mrule, vid, arity)@@A, NONE) =
        (Match(mrule, NONE)@@at(mrule), vid, arity)
    | Fmatch((mrule, vid@@A, arity)@@_, SOME((match, vid'@@_, arity')@@A')) =
        if vid <> vid' then
          Error.error(loc A, "inconsistent function identifier")
        else if arity <> arity' then
          Error.error(loc A', "inconsistent function arity")
        else
          (Match(mrule, SOME match)@@over(mrule, match), vid@@A, arity)

  fun Fmrule(op_opt, vid, atpats, ty_opt, exp) =
      let
        val pat = atPat(TUPLEAtPat(List.map atPat atpats)@@overAll(atpats))
        val exp =
            case ty_opt of
              NONE    => exp
            | SOME ty => COLONExp(exp, ty)@@over(ty, exp)
        val arity = List.length atpats
      in
        (Mrule(pat, exp)@@over(pat, exp), vid, arity)
      end


  (* Declarations [Figure 17] *)

  fun FUNDec(tyvarseq, fvalbind) =
        VALDec(tyvarseq, RECValBind(fvalbind)@@at(fvalbind))

  fun DATATYPEWITHTYPEDec(datbind, NONE) = DATATYPEDec(datbind)
    | DATATYPEWITHTYPEDec(datbind, SOME typbind) =
      let
        val datbind' = rewriteDatBind typbind datbind
        val dataDec  = DATATYPEDec(datbind')@@at(datbind')
        val typeDec  = TYPEDec(typbind)@@at(typbind)
      in
        SEQDec(dataDec, typeDec)
      end

  fun ABSTYPEWITHTYPEDec(datbind, NONE, dec) = ABSTYPEDec(datbind, dec)
    | ABSTYPEWITHTYPEDec(datbind, SOME typbind, dec) =
      let
        val datbind' = rewriteDatBind typbind datbind
        val typeDec  = TYPEDec(typbind)@@at(typbind)
      in
        ABSTYPEDec(datbind',
          SEQDec(typeDec, dec)@@over(typeDec, dec))
      end
end;
