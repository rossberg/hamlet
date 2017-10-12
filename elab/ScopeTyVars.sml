(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML scope of type variables
 * + RFC: Record extension
 * + RFC: Conjunctive patterns
 * + RFC: Disjunctive patterns
 * + RFC: Nested matches
 * + RFC: Views
 * + RFC: Simplified recursive value bindings
 * + RFC: Abstype as derived
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Definition, Section 4.6
 *)

structure ScopeTyVars : SCOPE_TYVARS =
struct
    (* Import *)

    open GrammarCore
    open GrammarModule
    type TyVarSet = TyVarSet.set


    (* Helpers *)

    val op+ = TyVarSet.union

    fun ?? tyvarsX  NONE    = TyVarSet.empty
      | ?? tyvarsX (SOME x) = tyvarsX x


    (* Operation *)

    fun unguardedTyVarsAtExp(SCONAtExp(_, _)) =
	    TyVarSet.empty
      | unguardedTyVarsAtExp(IDAtExp(_, _, _)) =
	    TyVarSet.empty
      | unguardedTyVarsAtExp(RECORDAtExp(_, exprow_opt)) =
	    ??unguardedTyVarsExpRow exprow_opt
      | unguardedTyVarsAtExp(LETAtExp(_, dec, exp)) =
	    unguardedTyVarsDec dec + unguardedTyVarsExp exp
      | unguardedTyVarsAtExp(PARAtExp(_, exp)) =
	    unguardedTyVarsExp exp

    (* [RFC: Record extension] *)
    and unguardedTyVarsExpRow(DOTSExpRow(_, exp)) =
	    unguardedTyVarsExp exp
      | unguardedTyVarsExpRow(FIELDExpRow(_, lab, exp, exprow_opt)) =
	    unguardedTyVarsExp exp + ??unguardedTyVarsExpRow exprow_opt

    and unguardedTyVarsExp(ATExp(_, atexp)) =
	    unguardedTyVarsAtExp atexp
      | unguardedTyVarsExp(APPExp(_, exp, atexp)) =
	    unguardedTyVarsExp exp + unguardedTyVarsAtExp atexp
      | unguardedTyVarsExp(COLONExp(_, exp, ty)) =
	    unguardedTyVarsExp exp + unguardedTyVarsTy ty
      (* [RFC: First-class modules] *)
      | unguardedTyVarsExp(PACKExp(_, _, _)) =
	    TyVarSet.empty
      | unguardedTyVarsExp(HANDLEExp(_, exp, match)) =
	    unguardedTyVarsExp exp + unguardedTyVarsMatch match
      | unguardedTyVarsExp(RAISEExp(_, exp)) =
	    unguardedTyVarsExp exp
      | unguardedTyVarsExp(FNExp(_, match)) =
	    unguardedTyVarsMatch match

    and unguardedTyVarsMatch(Match(_, mrule, match_opt)) =
	    unguardedTyVarsMrule mrule + ??unguardedTyVarsMatch match_opt

    and unguardedTyVarsMrule(Mrule(_, pat, exp)) =
	    unguardedTyVarsPat pat + unguardedTyVarsExp exp

    and unguardedTyVarsDec(VALDec(_, _, _, _)) =
	    TyVarSet.empty
      | unguardedTyVarsDec(TYPEDec(_, _)) =
	    TyVarSet.empty
      | unguardedTyVarsDec(DATATYPEDec(_, _)) =
	    TyVarSet.empty
      (* [RFC: Views] *)
      | unguardedTyVarsDec(VIEWTYPEDec(_, _, _, _, _, dec)) =
	    unguardedTyVarsDec dec
      | unguardedTyVarsDec(DATATYPE2Dec(_, _, _)) =
	    TyVarSet.empty
      (* Removed abstype [RFC: Abstype as derived] *)
      | unguardedTyVarsDec(EXCEPTIONDec(_, exbind)) =
	    unguardedTyVarsExBind exbind
      (* [RFC: Local modules] *)
      | unguardedTyVarsDec(STRDECDec(_, StrDec strdec)) =
	    unguardedTyVarsStrDec strdec
      | unguardedTyVarsDec(STRDECDec(_, _)) =
	    raise Fail "ScopeTyVars.unguardedTyVarsDec: invalid structure declaration"
      | unguardedTyVarsDec(LOCALDec(_, dec1, dec2)) =
	    unguardedTyVarsDec dec1 + unguardedTyVarsDec dec2
      | unguardedTyVarsDec(OPENDec(_, _)) =
	    TyVarSet.empty
      | unguardedTyVarsDec(EMPTYDec(_)) =
	    TyVarSet.empty
      | unguardedTyVarsDec(SEQDec(_, dec1, dec2)) =
	    unguardedTyVarsDec dec1 + unguardedTyVarsDec dec2

    (* [RFC: Simplified recursive value bindings] *)
    and unguardedTyVarsValBind(ValBind(_, pat, exp, valbind_opt)) =
	    unguardedTyVarsPat pat + unguardedTyVarsExp exp +
	    ??unguardedTyVarsValBind valbind_opt

    and unguardedTyVarsExBind(NEWExBind(_, _, vid, ty_opt, exbind_opt)) =
	    ??unguardedTyVarsTy ty_opt + ??unguardedTyVarsExBind exbind_opt
      | unguardedTyVarsExBind(EQUALExBind(_, _, vid, _, longvid, exbind_opt)) =
	    ??unguardedTyVarsExBind exbind_opt

    and unguardedTyVarsAtPat(WILDCARDAtPat(_)) =
	    TyVarSet.empty
      | unguardedTyVarsAtPat(SCONAtPat(_, _)) =
	    TyVarSet.empty
      | unguardedTyVarsAtPat(IDAtPat(_, _, _)) =
	    TyVarSet.empty
      | unguardedTyVarsAtPat(RECORDAtPat(_, patrow_opt)) =
	    ??unguardedTyVarsPatRow patrow_opt
      | unguardedTyVarsAtPat(PARAtPat(_, pat)) =
	    unguardedTyVarsPat pat

    (* [RFC: Record extension] *)
    and unguardedTyVarsPatRow(DOTSPatRow(_, pat)) =
	    unguardedTyVarsPat pat
      | unguardedTyVarsPatRow(FIELDPatRow(_, lab, pat, patrow_opt)) =
	    unguardedTyVarsPat pat + ??unguardedTyVarsPatRow patrow_opt

    and unguardedTyVarsPat(ATPat(_, atpat)) =
	    unguardedTyVarsAtPat atpat
      | unguardedTyVarsPat(CONPat(_, _, longvid, atpat)) =
	    unguardedTyVarsAtPat atpat
      | unguardedTyVarsPat(COLONPat(_, pat, ty)) =
	    unguardedTyVarsPat pat + unguardedTyVarsTy ty
      (* [RFC: Conjunctive patterns] *)
      | unguardedTyVarsPat(ASPat(_, pat1, pat2)) =
	    unguardedTyVarsPat pat1 + unguardedTyVarsPat pat2
      (* [RFC: Disjunctive patterns] *)
      | unguardedTyVarsPat(BARPat(_, pat1, pat2)) =
	    unguardedTyVarsPat pat1 + unguardedTyVarsPat pat2
      (* [RFC: Nested matches] *)
      | unguardedTyVarsPat(WITHPat(_, pat1, pat2, exp)) =
	    unguardedTyVarsPat pat1 + unguardedTyVarsPat pat2 +
	    unguardedTyVarsExp exp

    and unguardedTyVarsTy(VARTy(_, tyvar)) = TyVarSet.singleton tyvar
      | unguardedTyVarsTy(RECORDTy(_, tyrow_opt)) =
	    ??unguardedTyVarsTyRow tyrow_opt
      | unguardedTyVarsTy(CONTy(_, tyseq, longtycon)) =
	    unguardedTyVarsTyseq tyseq
      | unguardedTyVarsTy(ARROWTy(_, ty, ty')) =
	    unguardedTyVarsTy ty + unguardedTyVarsTy ty'
      (* [RFC: First-class modules] *)
      | unguardedTyVarsTy(PACKTy(_, _)) =
	    TyVarSet.empty
      | unguardedTyVarsTy(PARTy(_, ty)) =
	    unguardedTyVarsTy ty

    (* [RFX: Record extension] *)
    and unguardedTyVarsTyRow(DOTSTyRow(_, ty)) =
	    unguardedTyVarsTy ty
      | unguardedTyVarsTyRow(FIELDTyRow(_, lab, ty, tyrow_opt)) =
	    unguardedTyVarsTy ty + ??unguardedTyVarsTyRow tyrow_opt

    and unguardedTyVarsTyseq(Tyseq(_, tys)) =
	    List.foldl (fn(ty,U) => unguardedTyVarsTy ty + U) TyVarSet.empty tys

    (* [RFC: Local modules] *)
    and unguardedTyVarsStrDec(STRUCTUREStrDec(_, strbind)) =
	    unguardedTyVarsStrBind strbind
      | unguardedTyVarsStrDec(SIGNATUREStrDec(_, _)) =
	    TyVarSet.empty

    (* [RFC: Local modules] *)
    and unguardedTyVarsStrBind(StrBind(_, strid, strexp, strbind_opt)) =
	    unguardedTyVarsStrExp strexp + ??unguardedTyVarsStrBind strbind_opt

    (* [RFC: Local modules] *)
    and unguardedTyVarsStrExp(STRUCTStrExp(_, dec)) =
	    unguardedTyVarsDec dec
      | unguardedTyVarsStrExp(IDStrExp(_, _)) =
	    TyVarSet.empty
      | unguardedTyVarsStrExp(COLONStrExp(_, strexp, _)) =
	    unguardedTyVarsStrExp strexp
      | unguardedTyVarsStrExp(SEALStrExp(_, strexp, _)) =
	    unguardedTyVarsStrExp strexp
      (* [RFC: First-class modules] *)
      | unguardedTyVarsStrExp(UNPACKStrExp(_, atexp, _)) =
	    unguardedTyVarsAtExp atexp
      | unguardedTyVarsStrExp(APPStrExp(_, strexp1, strexp2)) =
	    unguardedTyVarsStrExp strexp1 + unguardedTyVarsStrExp strexp2
      | unguardedTyVarsStrExp(FCTStrExp(_, _, _, strexp)) =
	    unguardedTyVarsStrExp strexp
      | unguardedTyVarsStrExp(LETStrExp(_, dec, strexp)) =
	    unguardedTyVarsDec dec + unguardedTyVarsStrExp strexp
      | unguardedTyVarsStrExp(PARStrExp(_, strexp)) =
	    unguardedTyVarsStrExp strexp


    (* Export *)

    val unguardedTyVars = unguardedTyVarsValBind
end;
