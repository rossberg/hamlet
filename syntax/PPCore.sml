(*
 * (c) Andreas Rossberg 2007
 *
 * Printer for abstract core grammar
 *)

structure PPCore : PP_CORE =
struct
    (* Import *)

    open GrammarCore
    open PPGrammar


    (* Recursive import *)

    structure PPModule =
    struct
        val ppLongSigId :
	    (TextIO.outstream * int * longSigId -> unit) ref =
            ref (fn _ => raise Fail "PPCore.PPModule.ppLongSigId")
        val ppStrExp :
	    (TextIO.outstream * int * GrammarModule.StrExp -> unit) ref =
            ref (fn _ => raise Fail "PPCore.PPModule.ppStrExp")
        val ppStrDec : (TextIO.outstream * int * StrDec' -> unit) ref =
            ref (fn _ => raise Fail "PPCore.PPModule.ppStrDec")
    end


    (* Special constants *)

    fun ppSCon(out, i, scon) =
	let
	    val tag = case scon of SCon.INT _    => "INT"
				 | SCon.WORD _   => "WORD"
				 | SCon.STRING _ => "STRING"
				 | SCon.CHAR _   => "CHAR"
				 | SCon.REAL _   => "REAL"
	in
	    ppAtom(out, i, tag ^ "SCon", SCon.toString scon)
	end


    (* Identifiers *)

    fun ppLab(out, i, lab)     = ppAtom(out, i, "Lab", Lab.toString lab)
    fun ppVId(out, i, vid)     = ppAtom(out, i, "VId", VId.toString vid)
    fun ppTyVar(out, i, tyvar) = ppAtom(out, i, "TyVar", TyVar.toString tyvar)
    fun ppTyCon(out, i, tycon) = ppAtom(out, i, "TyCon", TyCon.toString tycon)
    fun ppStrId(out, i, strid) = ppAtom(out, i, "StrId", StrId.toString strid)

    fun ppLongVId(out, i, longvid) =
	    ppAtom(out, i, "LongVId", LongVId.toString longvid)
    fun ppLongTyCon(out, i, longtycon) =
	    ppAtom(out, i, "LongTyCon", LongTyCon.toString longtycon)
    fun ppLongStrId(out, i, longstrid) =
	    ppAtom(out, i, "LongStrId", LongStrId.toString longstrid)


    (* Expressions *)

    fun ppAtExp(out, i, SCONAtExp(I, scon)) =
	    ppElem(out, i, "SCONAtExp", I,
		   [sub ppSCon scon])
      | ppAtExp(out, i, IDAtExp(I, _, longvid)) =
	    ppElem(out, i, "IDAtExp", I,
		   [sub ppLongVId longvid])
      | ppAtExp(out, i, RECORDAtExp(I, exprow_opt)) =
	    ppElem(out, i, "RECORDAtExp", I,
		   [subo ppExpRow exprow_opt])
      | ppAtExp(out, i, LETAtExp(I, dec, exp)) =
	    ppElem(out, i, "LETAtExp", I,
		   [sub ppDec dec, sub ppExp exp])
      | ppAtExp(out, i, PARAtExp(I, exp)) =
	    ppElem(out, i, "PARAtExp", I,
		   [sub ppExp exp])

    and ppExpRow(out, i, DOTSExpRow(I, exp)) =
	    ppElem(out, i, "DOTSExpRow", I,
		   [sub ppExp exp])
      | ppExpRow(out, i, FIELDExpRow(I, lab, exp, exprow_opt)) =
	    ppElem(out, i, "FIELDExpRow", I,
		   [sub ppLab lab, sub ppExp exp, subo ppExpRow exprow_opt])

    and ppExp(out, i, ATExp(I, atexp)) =
	    ppElem(out, i, "ATExp", I,
		   [sub ppAtExp atexp])
      | ppExp(out, i, APPExp(I, exp, atexp)) =
	    ppElem(out, i, "APPExp", I,
		   [sub ppExp exp, sub ppAtExp atexp])
      | ppExp(out, i, COLONExp(I, exp, ty)) =
	    ppElem(out, i, "COLONExp", I,
		   [sub ppExp exp, sub ppTy ty])
      | ppExp(out, i, PACKExp(I, longstrid, longsigid)) =
	    ppElem(out, i, "PACKExp", I,
		   [sub ppLongStrId longstrid,
		    sub (!PPModule.ppLongSigId) longsigid])
      | ppExp(out, i, HANDLEExp(I, exp, match)) =
	    ppElem(out, i, "HANDLEExp", I,
		   [sub ppExp exp, sub ppMatch match])
      | ppExp(out, i, RAISEExp(I, exp)) =
	    ppElem(out, i, "RAISEExp", I,
		   [sub ppExp exp])
      | ppExp(out, i, FNExp(I, match)) =
	    ppElem(out, i, "FNExp", I,
		   [sub ppMatch match])


    (* Matches *)

    and ppMatch(out, i, Match(I, mrule, match_opt)) =
	    ppElem(out, i, "Match", I,
		   [sub ppMrule mrule, subo ppMatch match_opt])

    and ppMrule(out, i, Mrule(I, pat, exp)) =
	    ppElem(out, i, "Mrule", I,
		   [sub ppPat pat, sub ppExp exp])


    (* Declarations *)

    and ppRec(out, i, SANSRec) = ()
      | ppRec(out, i, WITHRec) = ppAtom(out, i, "Rec", "")

    and ppDec(out, i, VALDec(I, rec_opt, tyvarseq, valbind)) =
	    ppElem(out, i, "VALDec", I,
		   [sub ppRec rec_opt, sub ppTyVarseq tyvarseq,
		    sub ppValBind valbind])
      | ppDec(out, i, TYPEDec(I, typbind)) =
	    ppElem(out, i, "TYPEDec", I,
		   [sub ppTypBind typbind])
      | ppDec(out, i, DATATYPEDec(I, datbind)) =
	    ppElem(out, i, "DATATYPEDec", I,
		   [sub ppDatBind datbind])
      | ppDec(out, i, VIEWTYPEDec(I, tyvarseq, tycon, ty, conbind, dec)) =
	    ppElem(out, i, "VIEWTYPEDec", I,
		   [sub ppTyVarseq tyvarseq, sub ppTyCon tycon, sub ppTy ty,
		    sub ppConBind conbind, sub ppDec dec])
      | ppDec(out, i, DATATYPE2Dec(I, tycon, longtycon)) =
	    ppElem(out, i, "DATATYPE2Dec", I,
		   [sub ppTyCon tycon, sub ppLongTyCon longtycon])
      | ppDec(out, i, EXCEPTIONDec(I, exbind)) =
	    ppElem(out, i, "EXCEPTIONDec", I,
		   [sub ppExBind exbind])
      | ppDec(out, i, STRDECDec(I, strdec)) =
	    ppElem(out, i, "STRDECDec", I,
		   [sub (!PPModule.ppStrDec) strdec])
      | ppDec(out, i, LOCALDec(I, dec1, dec2)) =
	    ppElem(out, i, "LOCALDec", I,
		   [sub ppDec dec1, sub ppDec dec2])
      | ppDec(out, i, OPENDec(I, longstrids)) =
	    ppElem(out, i, "OPENDec", I,
		   [subs ppLongStrId longstrids])
      | ppDec(out, i, EMPTYDec(I)) =
	    ppElem(out, i, "EMPTYDec", I, [])
      | ppDec(out, i, SEQDec(I, dec1, dec2)) =
	    ppElem(out, i, "SEQDec", I,
		   [sub ppDec dec1, sub ppDec dec2])

    and ppValBind(out, i, ValBind(I, pat, exp, valbind_opt)) =
	    ppElem(out, i, "ValBind", I,
		   [sub ppPat pat, sub ppExp exp, subo ppValBind valbind_opt])
    and ppTypBind(out, i, TypBind(I, tyvarseq, tycon, ty, typbind_opt)) =
	    ppElem(out, i, "TypBind", I,
		   [sub ppTyVarseq tyvarseq, sub ppTyCon tycon, sub ppTy ty,
		    subo ppTypBind typbind_opt])
    and ppDatBind(out, i, DatBind(I, tyvarseq, tycon, conbind, datbind_opt)) =
	    ppElem(out, i, "DatBind", I,
		   [sub ppTyVarseq tyvarseq, sub ppTyCon tycon,
		    sub ppConBind conbind, subo ppDatBind datbind_opt])
    and ppConBind(out, i, ConBind(I, _, vid, ty_opt, conbind_opt)) =
	    ppElem(out, i, "ConBind", I,
		   [sub ppVId vid, subo ppTy ty_opt,
		    subo ppConBind conbind_opt])
    and ppExBind(out, i, NEWExBind(I, _, vid, ty_opt, exbind_opt)) =
	    ppElem(out, i, "NEWExBind", I,
		   [sub ppVId vid, subo ppTy ty_opt,
		    subo ppExBind exbind_opt])
      | ppExBind(out, i, EQUALExBind(I, _, vid, _, longvid, exbind_opt)) =
	    ppElem(out, i, "EQUALExBind", I,
		   [sub ppVId vid, sub ppLongVId longvid,
		    subo ppExBind exbind_opt])


    (* Patterns *)

    and ppAtPat(out, i, WILDCARDAtPat(I)) =
	    ppElem(out, i, "WILDCARDAtPat", I, [])
      | ppAtPat(out, i, SCONAtPat(I, scon)) =
	    ppElem(out, i, "SCONAtPat", I,
		   [sub ppSCon scon])
      | ppAtPat(out, i, IDAtPat(I, _, longvid)) =
	    ppElem(out, i, "IDAtPat", I,
		   [sub ppLongVId longvid])
      | ppAtPat(out, i, RECORDAtPat(I, patrow_opt)) =
	    ppElem(out, i, "RECORDAtPat", I,
		   [subo ppPatRow patrow_opt])
      | ppAtPat(out, i, PARAtPat(I, pat)) =
	    ppElem(out, i, "PARAtPat", I,
		   [sub ppPat pat])

    and ppPatRow(out, i, DOTSPatRow(I, pat)) =
	    ppElem(out, i, "DOTSPatRow", I,
		   [sub ppPat pat])
      | ppPatRow(out, i, FIELDPatRow(I, lab, pat, patrow_opt)) =
	    ppElem(out, i, "FIELDPatRow", I,
		   [sub ppLab lab, sub ppPat pat, subo ppPatRow patrow_opt])

    and ppPat(out, i, ATPat(I, atpat)) =
	    ppElem(out, i, "ATPat", I,
		   [sub ppAtPat atpat])
      | ppPat(out, i, CONPat(I, _, longvid, atpat)) =
	    ppElem(out, i, "CONPat", I,
		   [sub ppLongVId longvid, sub ppAtPat atpat])
      | ppPat(out, i, COLONPat(I, pat, ty)) =
	    ppElem(out, i, "COLONPat", I,
		   [sub ppPat pat, sub ppTy ty])
      | ppPat(out, i, ASPat(I, pat1, pat2)) =
	    ppElem(out, i, "ASPat", I,
		   [sub ppPat pat1, sub ppPat pat2])
      | ppPat(out, i, BARPat(I, pat1, pat2)) =
	    ppElem(out, i, "BARPat", I,
		   [sub ppPat pat1, sub ppPat pat2])
      | ppPat(out, i, WITHPat(I, pat1, pat2, exp)) =
	    ppElem(out, i, "WITHPat", I,
		   [sub ppPat pat1, sub ppPat pat2, sub ppExp exp])


    (* Type expressions *)

    and ppTy(out, i, VARTy(I, tyvar)) =
	    ppElem(out, i, "VARTy", I,
		   [sub ppTyVar tyvar])
      | ppTy(out, i, RECORDTy(I, tyrow_opt)) =
	    ppElem(out, i, "RECORDTy", I,
		   [subo ppTyRow tyrow_opt])
      | ppTy(out, i, CONTy(I, tyseq, longtycon)) =
	    ppElem(out, i, "CONTy", I,
		   [sub ppTyseq tyseq, sub ppLongTyCon longtycon])
      | ppTy(out, i, ARROWTy(I, ty1, ty2)) =
	    ppElem(out, i, "ARROWTy", I,
		   [sub ppTy ty1, sub ppTy ty2])
      | ppTy(out, i, PACKTy(I, longsigid)) =
	    ppElem(out, i, "PACKTy", I,
		   [sub (!PPModule.ppLongSigId) longsigid])
      | ppTy(out, i, PARTy(I, ty)) =
	    ppElem(out, i, "PARTy", I,
		   [sub ppTy ty])

    and ppTyRow(out, i, DOTSTyRow(I, ty)) =
	    ppElem(out, i, "DOTSTyRow", I,
		   [sub ppTy ty])
      | ppTyRow(out, i, FIELDTyRow(I, lab, ty, tyrow_opt)) =
	    ppElem(out, i, "FIELDTyRow", I,
		   [sub ppLab lab, sub ppTy ty, subo ppTyRow tyrow_opt])


    (* Sequences *)

    and ppTyseq(out, i, Tyseq(I, tys)) =
	    ppElem(out, i, "Tyseq", I,
		   [subs ppTy tys])

    and ppTyVarseq(out, i, TyVarseq(I, tyvars)) =
	    ppElem(out, i, "TyVarseq", I,
		   [subs ppTyVar tyvars])
end;
