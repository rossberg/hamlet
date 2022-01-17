(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML core derived forms
 *
 * Definition, Section 2.7 and Appendix A
 * + RFC: Syntax fixes
 * + RFC: Record punning
 * + RFC: Record extension
 * + RFC: Record update
 * + RFC: Conjunctive patterns
 * + RFC: Pattern guards
 * + RFC: Transformation patterns
 * + RFC: Views
 * + RFC: Optional else branch
 * + RFC: Do declarations
 * + RFC: Simplified recursive value bindings
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Notes:
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

    structure C    = GrammarCore
    structure M    = GrammarModule

    type Info      = C.Info

    type Lab       = C.Lab
    type VId       = C.VId
    type TyCon     = C.TyCon
    type longTyCon = C.longTyCon

    type Op        = C.Op
    type AtExp     = C.AtExp
    type ExpRow    = C.ExpRow
    type AppExp    = C.AtExp list
    type InfExp    = C.Exp
    type Exp       = C.Exp
    type Match     = C.Match
    type Mrule     = C.Mrule
    type Dec       = C.Dec
    type ValBind   = C.ValBind
    type FvalBind  = C.ValBind
    type Fmatch    = C.Match * C.VId * int
    type Fmrule    = C.Mrule * C.VId * int
    type TypBind   = C.TypBind
    type DatBind   = C.DatBind
    type AtPat     = C.AtPat
    type PatRow    = C.PatRow
    type AppPat    = C.AtPat list
    type InfPat    = C.Pat
    type Pat       = C.Pat
    type Ty        = C.Ty
    type TyRow     = C.TyRow
    type TyVarseq  = C.TyVarseq

    type StrExp    = M.StrExp
    type AtStrExp  = M.StrExp
    type AtSigExp  = M.SigExp


    (* Some helpers *)

    val vidFALSE               = VId.fromString "false"
    val vidTRUE                = VId.fromString "true"
    val vidNONE                = VId.fromString "NONE"
    val vidSOME                = VId.fromString "SOME"
    val vidNIL                 = VId.fromString "nil"
    val vidCONS                = VId.fromString "::"
    val longvidTRUE            = LongVId.fromId vidTRUE
    val longvidSOME            = LongVId.fromId vidSOME
    val longvidCONS            = LongVId.fromId vidCONS


    fun LONGVIDExp(I, longvid) = C.ATExp(I, C.IDAtExp(I, C.SANSOp, longvid))
    fun LONGVIDPat(I, longvid) = C.ATPat(I, C.IDAtPat(I, C.SANSOp, longvid))

    fun VIDAtExp(I, vid)       = C.IDAtExp(I, C.SANSOp, LongVId.fromId vid)
    fun VIDAtPat(I, vid)       = C.IDAtPat(I, C.SANSOp, LongVId.fromId vid)
    fun VIDExp(I, vid)         = LONGVIDExp(I, LongVId.fromId vid)
    fun VIDPat(I, vid)         = LONGVIDPat(I, LongVId.fromId vid)

    fun FALSEExp(I)            = VIDExp(I, vidFALSE)
    fun TRUEExp(I)             = VIDExp(I, vidTRUE)
    fun NILAtExp(I)            = VIDAtExp(I, vidNIL)
    fun CONSExp(I)             = VIDExp(I, vidCONS)

    fun FALSEPat(I)            = VIDPat(I, vidFALSE)
    fun TRUEPat(I)             = VIDPat(I, vidTRUE)
    fun NILAtPat(I)            = VIDAtPat(I, vidNIL)


    (* Rewriting of withtype declarations [Appendix A, 2nd bullet] *)

    fun findTyCon(tycon, C.TypBind(_, tyvarseq, tycon', ty, typbind_opt)) =
	    if tycon' = tycon then
		SOME(tyvarseq, ty)
	    else
		Option.mapPartial (fn typbind => findTyCon(tycon, typbind))
				  typbind_opt


    fun replaceTy (C.TyVarseq(_,tyvars), C.Tyseq(I',tys)) (C.VARTy(I,tyvar)) =
	let
	    fun loop(tyvar'::tyvars', ty'::tys') =
		    if tyvar' = tyvar then
			ty'
		    else
			loop(tyvars', tys')
	      | loop([],_) =
		    Error.error(I, "unbound type variable")
	      | loop(_,[]) =
		    Error.error(I', "type sequence has wrong arity")
	in
	    loop(tyvars, tys)
	end

      | replaceTy tyvarseq_tyseq (C.RECORDTy(I, tyrow_opt)) =
	    C.RECORDTy(I, Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)

      | replaceTy tyvarseq_tyseq (C.CONTy(I, tyseq', tycon)) =
	    C.CONTy(I, replaceTyseq tyvarseq_tyseq tyseq', tycon)

      | replaceTy tyvarseq_tyseq (C.ARROWTy(I, ty1, ty2)) =
	    C.ARROWTy(I, replaceTy tyvarseq_tyseq ty1,
			 replaceTy tyvarseq_tyseq ty2)

      | replaceTy tyvarseq_tyseq (ty as C.PACKTy _) = ty

      | replaceTy tyvarseq_tyseq (C.PARTy(I, ty)) =
	    C.PARTy(I, replaceTy tyvarseq_tyseq ty)

    (* [RFC: Record extension] *)
    and replaceTyRow tyvarseq_tyseq (C.DOTSTyRow(I, ty)) =
	    C.DOTSTyRow(I, replaceTy tyvarseq_tyseq ty)

      | replaceTyRow tyvarseq_tyseq (C.FIELDTyRow(I, lab, ty, tyrow_opt)) =
	    C.FIELDTyRow(I, lab, replaceTy tyvarseq_tyseq ty, 
			    Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)

    and replaceTyseq tyvarseq_tyseq (C.Tyseq(I, tys)) =	  
	    C.Tyseq(I, List.map (replaceTy tyvarseq_tyseq) tys)


    fun rewriteTy typbind (ty as C.VARTy _) = ty

      | rewriteTy typbind (C.RECORDTy(I, tyrow_opt)) =
	    C.RECORDTy(I, Option.map (rewriteTyRow typbind) tyrow_opt)

      | rewriteTy typbind (C.CONTy(I, tyseq, longtycon)) =
	let 
	    val tyseq'          = rewriteTyseq typbind tyseq
	    val (strids, tycon) = LongTyCon.explode longtycon
	in
	    if not(List.null strids) then
		C.CONTy(I, tyseq', longtycon)
	    else
		case findTyCon(tycon, typbind)
		  of SOME(tyvarseq', ty') => replaceTy (tyvarseq',tyseq') ty'
		   | NONE                 => C.CONTy(I, tyseq', longtycon)
	end

      | rewriteTy typbind (C.ARROWTy(I, ty1, ty2)) =
	    C.ARROWTy(I, rewriteTy typbind ty1, rewriteTy typbind ty2)

      | rewriteTy typbind (ty as C.PACKTy _) = ty

      | rewriteTy typbind (C.PARTy(I, ty)) =
	    C.PARTy(I, rewriteTy typbind ty)

    (* [RFC: Record extension] *)
    and rewriteTyRow typbind (C.DOTSTyRow(I, ty)) =
	    C.DOTSTyRow(I, rewriteTy typbind ty)

      | rewriteTyRow typbind (C.FIELDTyRow(I, lab, ty, tyrow_opt)) =
	    C.FIELDTyRow(I, lab, rewriteTy typbind ty,
			    Option.map (rewriteTyRow typbind) tyrow_opt)

    and rewriteTyseq typbind (C.Tyseq(I, tys)) =
	    C.Tyseq(I, List.map (rewriteTy typbind) tys)

    fun rewriteConBind typbind (C.ConBind(I, op_opt, vid, ty_opt, conbind_opt))=
	    C.ConBind(I, op_opt, vid,
			 Option.map (rewriteTy typbind) ty_opt,
			 Option.map (rewriteConBind typbind) conbind_opt)

    fun rewriteDatBind typbind (C.DatBind(I, tyvarseq, tycon, conbind,
							      datbind_opt)) =
	case findTyCon(tycon, typbind)
	  of NONE =>
	     C.DatBind(I, tyvarseq, tycon, rewriteConBind typbind conbind,
			  Option.map (rewriteDatBind typbind) datbind_opt)
	   | SOME _ =>
		Error.error(I, "duplicate type constructor \
			       \in recursive type declaration")

    (* Rewriting of abstype declarations [Appendix A, last bullet;
     *                                    RFC: Abstype as derived] *)

    fun obtainTyseqFromTyVarseq(C.TyVarseq(I, tyvars)) =
	    C.Tyseq(I, List.map (fn tyvar => C.VARTy(I, tyvar)) tyvars)

    fun obtainTypBindFromDatBind(C.DatBind(I, tyvarseq, tycon, conbind,
							       datbind_opt)) =
	    C.TypBind(I, tyvarseq, tycon,
		      C.CONTy(C.infoTyVarseq tyvarseq,
			      obtainTyseqFromTyVarseq tyvarseq,
			      LongTyCon.fromId tycon),
		      Option.map obtainTypBindFromDatBind datbind_opt)


    (* Patterns [Figure 16] *)

    fun UNITAtPat(I) = C.RECORDAtPat(I, NONE)

    fun TUPLEAtPat(I, [pat]) = C.PARAtPat(I, pat)
      | TUPLEAtPat(I,  pats) =
	let
	    fun toPatRow(n,    []     ) = NONE
	      | toPatRow(n, pat::pats') =
		SOME(C.FIELDPatRow(I, Lab.fromInt n, pat, toPatRow(n+1,pats')))
	in
	    C.RECORDAtPat(I, toPatRow(1, pats))
	end

    fun LISTAtPat(I, [])   = NILAtPat(I)
      | LISTAtPat(I, pats) =
	let
	    fun toPatList    []       = C.ATPat(I, NILAtPat(I))
	      | toPatList(pat::pats') =
		C.CONPat(I, C.SANSOp, longvidCONS,
			     TUPLEAtPat(I, [pat, toPatList pats']))
	in
	    C.PARAtPat(I, toPatList pats)
	end


    (* [RFC: Transformation patterns] *)
    fun QUESTAtPat(I, atexp) =
	let
	    val longvid = LongVId.fromId(VId.invent())
	    val pat1    = LONGVIDPat(I, longvid)
	    val pat2    = LONGVIDPat(I, longvidTRUE)
	    val exp     = C.APPExp(I, C.ATExp(C.infoAtExp atexp, atexp),
				      C.IDAtExp(I, C.SANSOp, longvid))
	in
	    C.PARAtPat(I, C.WITHPat(I, pat1, pat2, exp))
	end

    fun QUESTCONPat(I, atexp, atpat) =
	let
	    val longvid = LongVId.fromId(VId.invent())
	    val pat1    = LONGVIDPat(I, longvid)
	    val pat2    = C.CONPat(I, C.SANSOp, longvidSOME, atpat)
	    val exp     = C.APPExp(I, C.ATExp(C.infoAtExp atexp, atexp),
				      C.IDAtExp(I, C.SANSOp, longvid))
	in
	    C.WITHPat(I, pat1, pat2, exp)
	end

    (* [RFC: Pattern guards] *)
    fun IFPat(I, pat, exp) =
	let
	    val pat2 = LONGVIDPat(I, longvidTRUE)
	in
	    C.WITHPat(I, pat, pat2, exp)
	end


    (* Pattern Rows [Figure 16] *)

    (* [RFC: Record extension] *)
    fun DOTSPatRow(I, SOME pat, NONE) = C.DOTSPatRow(I, pat)
      | DOTSPatRow(I, NONE, NONE)     =
	    C.DOTSPatRow(I, C.ATPat(I, C.WILDCARDAtPat(I)))
      | DOTSPatRow(I, pat_opt, SOME(C.FIELDPatRow(I', lab, pat, patrow_opt))) =
	    C.FIELDPatRow(I', lab, pat,
			  SOME(DOTSPatRow(I, pat_opt, patrow_opt)))
      | DOTSPatRow(I, pat_opt, SOME(C.DOTSPatRow(I', _))) =
	    Error.error(I', "multiple ellipses in record pattern")

    fun IDPatRow(I, vid, ty_opt, pat_opt, patrow_opt) =
	let
	    val lab    = Lab.fromString(VId.toString vid)
	    val vidPat = VIDPat(I, vid)
	    val pat =
		case ty_opt
		  of NONE    => vidPat
		   | SOME ty => C.COLONPat(I, vidPat, ty)
	    val pat' =
		case pat_opt
		  of NONE      => pat
		   | SOME pat2 => C.ASPat(I, pat, pat2)
	in
	    C.FIELDPatRow(I, lab, pat', patrow_opt)
	end


    (* Expressions [Figure 15] *)

    fun UNITAtExp(I) = C.RECORDAtExp(I, NONE)

    fun TUPLEAtExp(I, [exp]) = C.PARAtExp(I, exp)
      | TUPLEAtExp(I,  exps) =
	let
	    fun toExpRow(n,    []     ) = NONE
	      | toExpRow(n, exp::exps') =
		SOME(C.FIELDExpRow(I, Lab.fromInt n, exp, toExpRow(n+1, exps')))
	in
	    C.RECORDAtExp(I, toExpRow(1, exps))
	end

    (* [RFC: Record update] *)
    fun UPDATEAtExp(I, atexp, exprow_opt) =
	let
	    val I'  = C.infoAtExp atexp
	    val vid = VId.invent()

	    fun toPatRow NONE =
		    SOME(C.DOTSPatRow(I', VIDPat(I', vid)))
	      | toPatRow(SOME(C.FIELDExpRow(I, lab, exp, exprow_opt))) =
		    SOME(C.FIELDPatRow(I, lab,
				C.ATPat(I, C.WILDCARDAtPat(C.infoExp exp)),
				toPatRow exprow_opt))
	      | toPatRow(SOME(C.DOTSExpRow(I, _))) =
		    Error.error(I, "invalid record update syntax")

	    fun extendExpRow NONE =
		    SOME(C.DOTSExpRow(I', VIDExp(I', vid)))
	      | extendExpRow(SOME(C.FIELDExpRow(I, lab, exp, exprow_opt))) =
		    SOME(C.FIELDExpRow(I, lab, exp, extendExpRow exprow_opt))
	      | extendExpRow(SOME(C.DOTSExpRow(I, _))) =
		    Error.error(I, "invalid record update syntax")

	    val pat = C.ATPat(I', C.RECORDAtPat(I', toPatRow(exprow_opt)))
	    val dec = C.VALDec(I', C.SANSRec, C.TyVarseq(I', []),
			       C.ValBind(I', pat, C.ATExp(I', atexp), NONE))
	    val exp = C.ATExp(I, C.RECORDAtExp(I, extendExpRow(exprow_opt)))
	in
	    C.LETAtExp(I, dec, exp)
	end

    fun HASHAtExp(I, lab) =
	let
	    val vid    = VId.invent()
	    val dots   = DOTSPatRow(I, NONE, NONE)
	    val patrow = C.FIELDPatRow(I, lab, VIDPat(I, vid), SOME dots)
	    val pat    = C.ATPat(I, C.RECORDAtPat(I, SOME patrow))
	    val mrule  = C.Mrule(I, pat, VIDExp(I, vid))
	    val match  = C.Match(I, mrule, NONE)
	in
	    C.PARAtExp(I, C.FNExp(I, match))
	end

    (* [RFC: First-class modules] *)
    fun PACKExp(I, M.IDStrExp(_, longstrid), M.IDSigExp(_, longsigid)) =
	    C.PACKExp(I, longstrid, longsigid)
      | PACKExp(I, atstrexp, atsigexp) =
	let
	    val I1      = M.infoStrExp atstrexp
	    val I2      = M.infoSigExp atsigexp
	    val strid   = StrId.invent()
	    val sigid   = SigId.invent()
	    val strbind = M.StrBind(I1, strid, atstrexp, NONE)
	    val sigbind = M.SigBind(I2, sigid, atsigexp, NONE)
	    val strdec1 = M.STRUCTUREStrDec(I1, strbind)
	    val strdec2 = M.SIGNATUREStrDec(I2, sigbind)
	    val dec1    = C.STRDECDec(I1, M.StrDec strdec1)
	    val dec2    = C.STRDECDec(I2, M.StrDec strdec2)
	    val dec     = C.SEQDec(Source.over(I1,I2), dec1, dec2)
	    val exp     = C.PACKExp(I, LongStrId.fromId strid,
				       LongSigId.fromId sigid)
	in
	    C.ATExp(I, C.LETAtExp(I, dec, exp))
	end

    fun CASEExp(I, exp, match) =
	let
	    val function = C.ATExp(I, C.PARAtExp(I, C.FNExp(I, match)))
	in
	    C.APPExp(I, function, C.PARAtExp(I, exp))
	end

    fun IFExp'(I, exp1, exp2, exp3) =
	let
	    val mruleTrue  = C.Mrule(I, TRUEPat(I), exp2)
	    val mruleFalse = C.Mrule(I, FALSEPat(I), exp3)
	    val matchFalse = C.Match(I, mruleFalse, NONE)
	    val matchTrue  = C.Match(I, mruleTrue, SOME matchFalse)
	in
	    CASEExp(I, exp1, matchTrue)
	end

    (* [RFC: Optional else branch] *)
    fun IFExp(I, exp1, exp2, SOME exp3) = IFExp'(I, exp1, exp2, exp3)
      | IFExp(I, exp1, exp2, NONE)      =
	    IFExp'(I, exp1, exp2, C.ATExp(I, UNITAtExp(I)))

    fun ORELSEExp (I, exp1, exp2) = IFExp'(I, exp1, TRUEExp(I), exp2)

    fun ANDALSOExp(I, exp1, exp2) = IFExp'(I, exp1, exp2, FALSEExp(I))

    fun SEQAtExp(I, exps) =
	let
	    val wildcard             = C.ATPat(I, C.WILDCARDAtPat(I))

	    fun toExpSeq []          = raise Fail "DerivedFormsCore.SEQAtExp: \
						  \empty exp list"
	      | toExpSeq [exp]       = exp
	      | toExpSeq(exp::exps') =
		  let
		      val mrule = C.Mrule(I, wildcard, toExpSeq exps')
		      val match = C.Match(I, mrule, NONE)
		  in
		      CASEExp(I, exp, match)
		  end
	in
	    C.PARAtExp(I, toExpSeq exps)
	end

    fun LETAtExp(I, dec, [exp]) = C.LETAtExp(I, dec, exp)
      | LETAtExp(I, dec,  exps) =
	    C.LETAtExp(I, dec, C.ATExp(I, SEQAtExp(I, exps)))

    fun WHILEExp(I, exp1, exp2) =
	let
	    val vid       = VId.invent()
	    val vidExp    = VIDExp(I, vid)
	    val unitAtExp = UNITAtExp(I)
	    val unitExp   = C.ATExp(I, unitAtExp)
	    val callVid   = C.APPExp(I, vidExp, unitAtExp)

	    val seqExp    = C.ATExp(I, SEQAtExp(I, [exp2, callVid]))
	    val fnBody    = IFExp'(I, exp1, seqExp, unitExp)
	    val mrule     = C.Mrule(I, C.ATPat(I,UNITAtPat(I)), fnBody)
	    val match     = C.Match(I, mrule, NONE)
	    val fnExp     = C.FNExp(I, match)
	    val valbind   = C.ValBind(I, VIDPat(I, vid), fnExp, NONE)
	    val dec       = C.VALDec(I, C.WITHRec, C.TyVarseq(I, []), valbind)
	in
	    C.ATExp(I, C.LETAtExp(I, dec, callVid))
	end

    fun LISTAtExp(I, [])   = NILAtExp(I)
      | LISTAtExp(I, exps) =
	let
	    fun toExpList    []       = C.ATExp(I, NILAtExp(I))
	      | toExpList(exp::exps') =
		C.APPExp(I, CONSExp(I), TUPLEAtExp(I, [exp, toExpList exps']))
	in
	    C.PARAtExp(I, toExpList exps)
	end

    (* [RFC: Record punning] *)
    fun IDExpRow(I, vid, ty_opt, exprow_opt) =
	let
	    val lab    = Lab.fromString(VId.toString vid)
	    val vidExp = VIDExp(I, vid)
	    val exp    =
		case ty_opt
		  of NONE    => vidExp
		   | SOME ty => C.COLONExp(I, vidExp, ty)
	in
	    C.FIELDExpRow(I, lab, exp, exprow_opt)
	end

    (* [RFC: Record extension] *)
    fun DOTSExpRow(I, exp, NONE) =
	    C.DOTSExpRow(I, exp)
      | DOTSExpRow(I, exp, exprow_opt) =
	let
	    val I'     = C.infoExp exp
	    val vid    = VId.invent()
	    val dec    = C.VALDec(I', C.SANSRec, C.TyVarseq(I, []),
				  C.ValBind(I', VIDPat(I', vid), exp, NONE))
	    val exprow = DOTSExpRow'(I, vid, exprow_opt)
	    val recExp = C.ATExp(I, C.RECORDAtExp(I, SOME exprow))
	    val letExp = C.ATExp(I, C.LETAtExp(I, dec, recExp))
	in
	    C.DOTSExpRow(I, letExp)
	end
    and DOTSExpRow'(I, vid, NONE) =
	    C.DOTSExpRow(I, VIDExp(I, vid))
      | DOTSExpRow'(I, vid, SOME(C.FIELDExpRow(I', lab, exp, exprow_opt))) =
	    C.FIELDExpRow(I', lab, exp, SOME(DOTSExpRow'(I, vid, exprow_opt)))
      | DOTSExpRow'(I, vid, SOME(C.DOTSExpRow(I', _))) =
	    Error.error(I', "multiple ellipses in record expression")


    (* Type Expressions [Figure 16] *)

    fun TUPLETy(I, [ty]) = ty
      | TUPLETy(I,  tys) =
	let
	    fun toTyRow(n,   []    ) = NONE
	      | toTyRow(n, ty::tys') =
		SOME(C.FIELDTyRow(I, Lab.fromInt n, ty, toTyRow(n+1, tys')))
	in
	    C.RECORDTy(I, toTyRow(1, tys))
	end

    (* [RFC: Record extension] *)
    fun DOTSTyRow(I, ty, NONE) =
	    C.DOTSTyRow(I, ty)
      | DOTSTyRow(I, ty, SOME(C.FIELDTyRow(I', lab, ty', tyrow_opt))) =
	    C.FIELDTyRow(I', lab, ty', SOME(DOTSTyRow(I, ty, tyrow_opt)))
      | DOTSTyRow(I, ty, SOME(C.DOTSTyRow(I', _))) =
	    Error.error(I', "multiple ellipses in record type")


    (* Function-value Bindings [Figure 17] *)

    fun FvalBind(I, (match, vid, arity), fvalbind_opt) =
	let
	    fun abstract(0, vidExps) =
		let
		    val exp = C.ATExp(I, TUPLEAtExp(I, List.rev vidExps))
		in
		    CASEExp(I, exp, match)
		end

	      | abstract(n, vidExps) =
		let
		    val vid   = VId.invent()
		    val exp   = VIDExp(I, vid)
		    val pat   = VIDPat(I, vid)
		    val mrule = C.Mrule(I, pat, abstract(n-1, exp::vidExps))
		in
		    C.FNExp(I, C.Match(I, mrule, NONE))
		end

	    val exp = abstract(arity, [])
	    val pat = VIDPat(I, vid)
	in
	    C.ValBind(I, pat, exp, fvalbind_opt)
	end


    (* Function Matches [Figure 17; RFC: Syntax fixes] *)

    fun Fmatch(I, (mrule, vid, arity), NONE) =
	    ( C.Match(I, mrule, NONE), vid, arity )

      | Fmatch(I, (mrule, vid, arity), SOME(match, vid', arity')) =
	    if vid <> vid' then
		Error.error(I, "inconsistent function identifier")
	    else if arity <> arity' then
		Error.error(I, "inconsistent function arity")
	    else
		( C.Match(I, mrule, SOME match), vid, arity )


    (* Function Match Rules [Figure 17; RFC: Syntax fixes] *)

    fun Fmrule(I, _, vid, atpats, ty_opt, atexp_opt, exp) =
	let
	    val pats = List.map (fn atpat => C.ATPat(I, atpat)) atpats
	    val pat' = C.ATPat(I, TUPLEAtPat(I, pats))
	    (* [RFC: Pattern guards] *)
	    val pat'' =
		case atexp_opt
		  of NONE => pat'
		   | SOME atexp =>
		     IFPat(I, pat', C.ATExp(C.infoAtExp atexp, atexp))
	    (* [RFC: Syntax fixes] *)
	    val exp' = case ty_opt
			 of NONE    => exp
			  | SOME ty => C.COLONExp(I, exp, ty)
	    val arity = List.length atpats
	in
	    ( C.Mrule(I, pat'', exp'), vid, arity )
	end


    (* Declarations [Figure 17] *)

    (* [RFC: Do declarations] *)
    fun DODec(I, exp) =
	let
	    val pat = C.ATPat(I, TUPLEAtPat(I, []))
	in
	    C.VALDec(I, C.SANSRec, C.TyVarseq(I, []),
			C.ValBind(I, pat, exp, NONE))
	end

    fun FUNDec(I, tyvarseq, fvalbind) =
	    (* [RFC: Simplified recursive value bindings] *)
	    C.VALDec(I, C.WITHRec, tyvarseq, fvalbind)

    fun DATATYPEDec(I, datbind, NONE)         = C.DATATYPEDec(I, datbind)
      | DATATYPEDec(I, datbind, SOME typbind) =
	let
	    val datbind' = rewriteDatBind typbind datbind
	in
	    C.SEQDec(I, C.DATATYPEDec(C.infoDatBind datbind, datbind'),
			C.TYPEDec(C.infoTypBind typbind, typbind))
	end

    (* [RFC: Views] *)
    val VIEWTYPE2Dec = C.DATATYPE2Dec

    fun ABSTYPEDec(I, datbind, NONE, dec) =
	let
	    val typbind' = obtainTypBindFromDatBind datbind
	in
	    C.LOCALDec(I, C.DATATYPEDec(C.infoDatBind datbind, datbind),
			  C.SEQDec(I, C.TYPEDec(I, typbind'), dec))
	end

      | ABSTYPEDec(I, datbind, SOME typbind, dec) =
	let
	    val I'       = C.infoTypBind typbind
	    val datbind' = rewriteDatBind typbind datbind
	in
	    ABSTYPEDec(I, datbind', NONE,
		       C.SEQDec(I, C.TYPEDec(I', typbind), dec))
	end

    (* [RFC: Local modules] *)
    fun OPENDec(I, strexp) =
	let
	    exception NonList

	    fun longstrid(M.IDStrExp(_, longstrid)) = longstrid
	      | longstrid _ = raise NonList

	    fun longstrids(M.APPStrExp(_, strexp1, strexp2)) =
		    longstrids strexp1 @ [longstrid strexp2] 
	      | longstrids strexp = [longstrid strexp]
	in
	    C.OPENDec(I, longstrids strexp)
	    handle NonList =>
	    let
	        val strid   = StrId.invent()
	        val strbind = M.StrBind(I, strid, strexp, NONE)
	        val strdec  = M.STRUCTUREStrDec(I, strbind)
	        val dec1    = C.STRDECDec(I, M.StrDec strdec)
	        val dec2    = C.OPENDec(I, [LongStrId.fromId strid])
	    in
	        C.LOCALDec(I, dec1, dec2)
	    end
	end
end;
