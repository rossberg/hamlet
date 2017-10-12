(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Printer for abstract core syntax
 *)

structure PPCore : PP_CORE =
struct
  (* Import *)

  open SyntaxCore
  open Annotation
  open PPSyntax


  (* Special constants *)

  fun ppSCon(out, i, scon@@_) =
      let
        val tag = 
            case scon of
              SCon.INT _    => "INT"
            | SCon.WORD _   => "WORD"
            | SCon.STRING _ => "STRING"
            | SCon.CHAR _   => "CHAR"
            | SCon.REAL _   => "REAL"
      in
        ppAtom(out, i, tag ^ "SCon", SCon.toString scon)
      end


  (* Identifiers *)

  fun ppLab(out, i, lab@@_) = ppAtom(out, i, "Lab", Lab.toString lab)
  fun ppVId(out, i, vid@@_) = ppAtom(out, i, "VId", VId.toString vid)
  fun ppTyVar(out, i, tyvar@@_) = ppAtom(out, i, "TyVar", TyVar.toString tyvar)
  fun ppTyCon(out, i, tycon@@_) = ppAtom(out, i, "TyCon", TyCon.toString tycon)
  fun ppStrId(out, i, strid@@_) = ppAtom(out, i, "StrId", StrId.toString strid)

  fun ppLongVId(out, i, longvid@@_) =
      ppAtom(out, i, "LongVId", LongVId.toString longvid)
  fun ppLongTyCon(out, i, longtycon@@_) =
      ppAtom(out, i, "LongTyCon", LongTyCon.toString longtycon)
  fun ppLongStrId(out, i, longstrid@@_) =
      ppAtom(out, i, "LongStrId", LongStrId.toString longstrid)


  (* Expressions *)

  fun ppAtExp(out, i, SCONAtExp(scon)@@A) =
        ppElem(out, i, "SCONAtExp", A, [sub ppSCon scon])
    | ppAtExp(out, i, IDAtExp(op_opt, longvid)@@A) =
        ppElem(out, i, "IDAtExp", A, [sub ppLongVId longvid])
    | ppAtExp(out, i, RECORDAtExp(exprow_opt)@@A) =
        ppElem(out, i, "RECORDAtExp", A, [subo ppExpRow exprow_opt])
    | ppAtExp(out, i, LETAtExp(dec, exp)@@A) =
        ppElem(out, i, "LETAtExp", A, [sub ppDec dec, sub ppExp exp])
    | ppAtExp(out, i, PARAtExp(exp)@@A) =
        ppElem(out, i, "PARAtExp", A, [sub ppExp exp])

  and ppExpRow(out, i, ExpRow(lab, exp, exprow_opt)@@A) =
        ppElem(out, i, "ExpRow", A,
          [sub ppLab lab, sub ppExp exp, subo ppExpRow exprow_opt])

  and ppExp(out, i, ATExp(atexp)@@A) =
        ppElem(out, i, "ATExp", A, [sub ppAtExp atexp])
    | ppExp(out, i, APPExp(exp, atexp)@@A) =
        ppElem(out, i, "APPExp", A, [sub ppExp exp, sub ppAtExp atexp])
    | ppExp(out, i, COLONExp(exp, ty)@@A) =
        ppElem(out, i, "COLONExp", A, [sub ppExp exp, sub ppTy ty])
    | ppExp(out, i, HANDLEExp(exp, match)@@A) =
        ppElem(out, i, "HANDLEExp", A, [sub ppExp exp, sub ppMatch match])
    | ppExp(out, i, RAISEExp(exp)@@A) =
        ppElem(out, i, "RAISEExp", A, [sub ppExp exp])
    | ppExp(out, i, FNExp(match)@@A) =
        ppElem(out, i, "FNExp", A, [sub ppMatch match])


  (* Matches *)

  and ppMatch(out, i, Match(mrule, match_opt)@@A) =
        ppElem(out, i, "Match", A, [sub ppMrule mrule, subo ppMatch match_opt])

  and ppMrule(out, i, Mrule(pat, exp)@@A) =
        ppElem(out, i, "Mrule", A, [sub ppPat pat, sub ppExp exp])


  (* Declarations *)

  and ppDec(out, i, VALDec(tyvarseq, valbind)@@A) =
        ppElem(out, i, "VALDec", A,
          [sub ppTyVarseq tyvarseq, sub ppValBind valbind])
    | ppDec(out, i, TYPEDec(typbind)@@A) =
        ppElem(out, i, "TYPEDec", A, [sub ppTypBind typbind])
    | ppDec(out, i, DATATYPEDec(datbind)@@A) =
        ppElem(out, i, "DATATYPEDec", A, [sub ppDatBind datbind])
    | ppDec(out, i, DATATYPE2Dec(tycon, longtycon)@@A) =
        ppElem(out, i, "DATATYPE2Dec", A,
          [sub ppTyCon tycon, sub ppLongTyCon longtycon])
    | ppDec(out, i, ABSTYPEDec(datbind, dec)@@A) =
        ppElem(out, i, "ABSTYPEDec", A, [sub ppDatBind datbind, sub ppDec dec])
    | ppDec(out, i, EXCEPTIONDec(exbind)@@A) =
        ppElem(out, i, "EXCEPTIONDec", A, [sub ppExBind exbind])
    | ppDec(out, i, LOCALDec(dec1, dec2)@@A) =
        ppElem(out, i, "LOCALDec", A, [sub ppDec dec1, sub ppDec dec2])
    | ppDec(out, i, OPENDec(longstrids)@@A) =
        ppElem(out, i, "OPENDec", A, [subs ppLongStrId longstrids])
    | ppDec(out, i, EMPTYDec@@A) =
        ppElem(out, i, "EMPTYDec", A, [])
    | ppDec(out, i, SEQDec(dec1, dec2)@@A) =
        ppElem(out, i, "SEQDec", A, [sub ppDec dec1, sub ppDec dec2])

  and ppValBind(out, i, PLAINValBind(pat, exp, valbind_opt)@@A) =
        ppElem(out, i, "PLAINValBind", A,
          [sub ppPat pat, sub ppExp exp, subo ppValBind valbind_opt])
    | ppValBind(out, i, RECValBind(valbind)@@A) =
        ppElem(out, i, "RECValBind", A, [sub ppValBind valbind])

  and ppTypBind(out, i, TypBind(tyvarseq, tycon, ty, typbind_opt)@@A) =
        ppElem(out, i, "TypBind", A,
          [sub ppTyVarseq tyvarseq, sub ppTyCon tycon, sub ppTy ty,
            subo ppTypBind typbind_opt])

  and ppDatBind(out, i, DatBind(tyvarseq, tycon, conbind, datbind_opt)@@A) =
        ppElem(out, i, "DatBind", A,
          [sub ppTyVarseq tyvarseq, sub ppTyCon tycon,
            sub ppConBind conbind, subo ppDatBind datbind_opt])

  and ppConBind(out, i, ConBind(op_opt, vid, ty_opt, conbind_opt)@@A) =
        ppElem(out, i, "ConBind", A,
          [sub ppVId vid, subo ppTy ty_opt, subo ppConBind conbind_opt])

  and ppExBind(out, i, NEWExBind(op_opt, vid, ty_opt, exbind_opt)@@A) =
        ppElem(out, i, "NEWExBind", A,
          [sub ppVId vid, subo ppTy ty_opt, subo ppExBind exbind_opt])
    | ppExBind(out, i,
        EQUALExBind(op_opt, vid, op_opt', longvid, exbind_opt)@@A) =
        ppElem(out, i, "EQUALExBind", A,
          [sub ppVId vid, sub ppLongVId longvid, subo ppExBind exbind_opt])


  (* Patterns *)

  and ppAtPat(out, i, WILDCARDAtPat@@A) =
        ppElem(out, i, "WILDCARDAtPat", A, [])
    | ppAtPat(out, i, SCONAtPat(scon)@@A) =
        ppElem(out, i, "SCONAtPat", A, [sub ppSCon scon])
    | ppAtPat(out, i, IDAtPat(op_opt, longvid)@@A) =
        ppElem(out, i, "IDAtPat", A, [sub ppLongVId longvid])
    | ppAtPat(out, i, RECORDAtPat(patrow_opt)@@A) =
        ppElem(out, i, "RECORDAtPat", A, [subo ppPatRow patrow_opt])
    | ppAtPat(out, i, PARAtPat(pat)@@A) =
        ppElem(out, i, "PARAtPat", A, [sub ppPat pat])

  and ppPatRow(out, i, DOTSPatRow@@A) =
        ppElem(out, i, "DOTSPatRow", A, [])
    | ppPatRow(out, i, FIELDPatRow(lab, pat, patrow_opt)@@A) =
        ppElem(out, i, "FIELDPatRow", A,
          [sub ppLab lab, sub ppPat pat, subo ppPatRow patrow_opt])

  and ppPat(out, i, ATPat(atpat)@@A) =
        ppElem(out, i, "ATPat", A, [sub ppAtPat atpat])
    | ppPat(out, i, CONPat(op_opt, longvid, atpat)@@A) =
        ppElem(out, i, "CONPat", A, [sub ppLongVId longvid, sub ppAtPat atpat])
    | ppPat(out, i, COLONPat(pat, ty)@@A) =
        ppElem(out, i, "COLONPat", A, [sub ppPat pat, sub ppTy ty])
    | ppPat(out, i, ASPat(op_opt, vid, ty_opt, pat)@@A) =
        ppElem(out, i, "ASPat", A,
          [sub ppVId vid, subo ppTy ty_opt, sub ppPat pat])


  (* Type expressions *)

  and ppTy(out, i, VARTy(tyvar)@@A) =
        ppElem(out, i, "VARTy", A, [sub ppTyVar tyvar])
    | ppTy(out, i, RECORDTy(tyrow_opt)@@A) =
        ppElem(out, i, "RECORDTy", A, [subo ppTyRow tyrow_opt])
    | ppTy(out, i, CONTy(tyseq, longtycon)@@A) =
        ppElem(out, i, "CONTy", A,
          [sub ppTyseq tyseq, sub ppLongTyCon longtycon])
    | ppTy(out, i, ARROWTy(ty1, ty2)@@A) =
        ppElem(out, i, "ARROWTy", A, [sub ppTy ty1, sub ppTy ty2])
    | ppTy(out, i, PARTy(ty)@@A) =
        ppElem(out, i, "PARTy", A, [sub ppTy ty])

  and ppTyRow(out, i, TyRow(lab, ty, tyrow_opt)@@A) =
        ppElem(out, i, "TyRow", A,
          [sub ppLab lab, sub ppTy ty, subo ppTyRow tyrow_opt])


  (* Sequences *)

  and ppTyseq(out, i, Seq(tys)@@A) =
        ppElem(out, i, "Seq", A, [subs ppTy tys])

  and ppTyVarseq(out, i, Seq(tyvars)@@A) =
        ppElem(out, i, "Seq", A, [subs ppTyVar tyvars])
end;
