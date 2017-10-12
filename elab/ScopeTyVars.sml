(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML scope of type variables
 *
 * Definition, Section 4.6
 *)

structure ScopeTyVars : SCOPE_TYVARS =
struct
  (* Import *)

  open SyntaxCore
  open Annotation

  type TyVarSet = TyVarSet.set


  (* Helpers *)

  val op+ = TyVarSet.union

  fun ?tyvarsX NONE    = TyVarSet.empty
    | ?tyvarsX(SOME x) = tyvarsX x


  (* Operation *)

  fun unguardedTyVarsAtExp(SCONAtExp(_)@@_) =
        TyVarSet.empty
    | unguardedTyVarsAtExp(IDAtExp(_, _)@@_) =
        TyVarSet.empty
    | unguardedTyVarsAtExp(RECORDAtExp(exprow_opt)@@_) =
        ?unguardedTyVarsExpRow exprow_opt
    | unguardedTyVarsAtExp(LETAtExp(dec, exp)@@_) =
        unguardedTyVarsDec dec + unguardedTyVarsExp exp
    | unguardedTyVarsAtExp(PARAtExp(exp)@@_) =
        unguardedTyVarsExp exp

  and unguardedTyVarsExpRow(ExpRow(lab, exp, exprow_opt)@@_) =
        unguardedTyVarsExp exp + ?unguardedTyVarsExpRow exprow_opt

  and unguardedTyVarsExp(ATExp(atexp)@@_) =
        unguardedTyVarsAtExp atexp
    | unguardedTyVarsExp(APPExp(exp, atexp)@@_) =
        unguardedTyVarsExp exp + unguardedTyVarsAtExp atexp
    | unguardedTyVarsExp(COLONExp(exp, ty)@@_) =
        unguardedTyVarsExp exp + unguardedTyVarsTy ty
    | unguardedTyVarsExp(HANDLEExp(exp, match)@@_) =
        unguardedTyVarsExp exp + unguardedTyVarsMatch match
    | unguardedTyVarsExp(RAISEExp(exp)@@_) =
        unguardedTyVarsExp exp
    | unguardedTyVarsExp(FNExp(match)@@_) =
        unguardedTyVarsMatch match

  and unguardedTyVarsMatch(Match(mrule, match_opt)@@_) =
        unguardedTyVarsMrule mrule + ?unguardedTyVarsMatch match_opt

  and unguardedTyVarsMrule(Mrule(pat, exp)@@_) =
        unguardedTyVarsPat pat + unguardedTyVarsExp exp

  and unguardedTyVarsDec(VALDec(_, _)@@_) =
        TyVarSet.empty
    | unguardedTyVarsDec(TYPEDec(_)@@_) =
        TyVarSet.empty
    | unguardedTyVarsDec(DATATYPEDec(_)@@_) =
        TyVarSet.empty
    | unguardedTyVarsDec(DATATYPE2Dec(_, _)@@_) =
        TyVarSet.empty
    | unguardedTyVarsDec(ABSTYPEDec(datbind, dec)@@_) =
        unguardedTyVarsDec dec
    | unguardedTyVarsDec(EXCEPTIONDec(exbind)@@_) =
        unguardedTyVarsExBind exbind
    | unguardedTyVarsDec(LOCALDec(dec1, dec2)@@_) =
        unguardedTyVarsDec dec1 + unguardedTyVarsDec dec2
    | unguardedTyVarsDec(OPENDec(_)@@_) =
        TyVarSet.empty
    | unguardedTyVarsDec(EMPTYDec@@_) =
        TyVarSet.empty
    | unguardedTyVarsDec(SEQDec(dec1, dec2)@@_) =
        unguardedTyVarsDec dec1 + unguardedTyVarsDec dec2

  and unguardedTyVarsValBind(PLAINValBind(pat, exp, valbind_opt)@@_) =
        unguardedTyVarsPat pat + unguardedTyVarsExp exp +
          ?unguardedTyVarsValBind valbind_opt
    | unguardedTyVarsValBind(RECValBind(valbind)@@_) =
        unguardedTyVarsValBind valbind

  and unguardedTyVarsExBind(NEWExBind(_, vid, ty_opt, exbind_opt)@@_) =
        ?unguardedTyVarsTy ty_opt + ?unguardedTyVarsExBind exbind_opt
    | unguardedTyVarsExBind(EQUALExBind(_, vid, _, longvid, exbind_opt)@@_) =
        ?unguardedTyVarsExBind exbind_opt

  and unguardedTyVarsAtPat(WILDCARDAtPat@@_) =
        TyVarSet.empty
    | unguardedTyVarsAtPat(SCONAtPat(_)@@_) =
        TyVarSet.empty
    | unguardedTyVarsAtPat(IDAtPat(_, _)@@_) =
        TyVarSet.empty
    | unguardedTyVarsAtPat(RECORDAtPat(patrow_opt)@@_) =
        ?unguardedTyVarsPatRow patrow_opt
    | unguardedTyVarsAtPat(PARAtPat(pat)@@_) =
        unguardedTyVarsPat pat

  and unguardedTyVarsPatRow(DOTSPatRow@@_) =
        TyVarSet.empty
    | unguardedTyVarsPatRow(FIELDPatRow(lab, pat, patrow_opt)@@_) =
        unguardedTyVarsPat pat + ?unguardedTyVarsPatRow patrow_opt

  and unguardedTyVarsPat(ATPat(atpat)@@_) =
        unguardedTyVarsAtPat atpat
    | unguardedTyVarsPat(CONPat(_, longvid, atpat)@@_) =
        unguardedTyVarsAtPat atpat
    | unguardedTyVarsPat(COLONPat(pat, ty)@@_) =
        unguardedTyVarsPat pat + unguardedTyVarsTy ty
    | unguardedTyVarsPat(ASPat(_, vid, ty_opt, pat)@@_) =
        ?unguardedTyVarsTy ty_opt + unguardedTyVarsPat pat

  and unguardedTyVarsTy(VARTy(tyvar@@_)@@_) =
        TyVarSet.singleton tyvar
    | unguardedTyVarsTy(RECORDTy(tyrow_opt)@@_) =
        ?unguardedTyVarsTyRow tyrow_opt
    | unguardedTyVarsTy(CONTy(tyseq, longtycon)@@_) =
        unguardedTyVarsTyseq tyseq
    | unguardedTyVarsTy(ARROWTy(ty, ty')@@_) =
        unguardedTyVarsTy ty + unguardedTyVarsTy ty'
    | unguardedTyVarsTy(PARTy(ty)@@_) =
        unguardedTyVarsTy ty

  and unguardedTyVarsTyRow(TyRow(lab, ty, tyrow_opt)@@_) =
        unguardedTyVarsTy ty + ?unguardedTyVarsTyRow tyrow_opt

  and unguardedTyVarsTyseq(Seq(tys)@@_) =
        List.foldl (fn(ty, U) => unguardedTyVarsTy ty + U) TyVarSet.empty tys


  (* Export *)

  val unguardedTyVars = unguardedTyVarsValBind
end;
