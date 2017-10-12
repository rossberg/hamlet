(*
 * (c) Andreas Rossberg 2007
 *
 * Printer for abstract module grammar
 *)

structure PPModule : PP_MODULE =
struct
    (* Import *)

    open GrammarModule
    open PPGrammar


    (* Identifiers *)

    fun ppSigId(out, i, sigid) = ppAtom(out, i, "SigId", SigId.toString sigid)

    fun ppLongSigId(out, i, longsigid) =
	    ppAtom(out, i, "LongSigId", LongSigId.toString longsigid)


    (* Structures *)

    fun ppStrExp(out, i, STRUCTStrExp(I, dec)) =
	    ppElem(out, i, "STRUCTStrExp", I,
		   [sub PPCore.ppDec dec])
      | ppStrExp(out, i, IDStrExp(I, longstrid)) =
	    ppElem(out, i, "IDStrExp", I,
		   [sub PPCore.ppLongStrId longstrid])
      | ppStrExp(out, i, COLONStrExp(I, strexp, sigexp)) =
	    ppElem(out, i, "COLONStrExp", I,
		   [sub ppStrExp strexp, sub ppSigExp sigexp])
      | ppStrExp(out, i, SEALStrExp(I, strexp, sigexp)) =
	    ppElem(out, i, "SEALStrExp", I,
		   [sub ppStrExp strexp, sub ppSigExp sigexp])
      | ppStrExp(out, i, UNPACKStrExp(I, atexp, sigexp)) =
	    ppElem(out, i, "UNPACKStrExp", I,
		   [sub PPCore.ppAtExp atexp, sub ppSigExp sigexp])
      | ppStrExp(out, i, APPStrExp(I, strexp1, strexp2)) =
	    ppElem(out, i, "APPStrExp", I,
		   [sub ppStrExp strexp1, sub ppStrExp strexp2])
      | ppStrExp(out, i, LETStrExp(I, dec, strexp)) =
	    ppElem(out, i, "LETStrExp", I,
		   [sub PPCore.ppDec dec, sub ppStrExp strexp])
      | ppStrExp(out, i, FCTStrExp(I, strid, sigexp, strexp)) =
	    ppElem(out, i, "FCTStrExp", I,
		   [sub PPCore.ppStrId strid, sub ppSigExp sigexp,
		    sub ppStrExp strexp])
      | ppStrExp(out, i, PARStrExp(I, strexp)) =
	    ppElem(out, i, "PARStrExp", I,
		   [sub ppStrExp strexp])

    and ppStrDec(out, i, STRUCTUREStrDec(I, strbind)) =
	    ppElem(out, i, "STRUCTUREStrDec", I,
		   [sub ppStrBind strbind])
      | ppStrDec(out, i, SIGNATUREStrDec(I, sigbind)) =
	    ppElem(out, i, "SIGNATUREStrDec", I,
		   [sub ppSigBind sigbind])

    and ppStrBind(out, i, StrBind(I, strid, strexp, strbind_opt)) =
	    ppElem(out, i, "StrBind", I,
		   [sub PPCore.ppStrId strid, sub ppStrExp strexp,
		    subo ppStrBind strbind_opt])


    (* Signatures *)

    and ppSigExp(out, i, SIGSigExp(I, spec)) =
	    ppElem(out, i, "SIGSigExp", I,
		   [sub ppSpec spec])
      | ppSigExp(out, i, IDSigExp(I, longsigid)) =
	    ppElem(out, i, "IDSigExp", I,
		   [sub ppLongSigId longsigid])
      | ppSigExp(out, i, WHERETYPESigExp(I, sigexp, tyvarseq, longtycon, ty)) =
	    ppElem(out, i, "WHERETYPESigExp", I,
		   [sub ppSigExp sigexp, sub PPCore.ppTyVarseq tyvarseq,
		    sub PPCore.ppLongTyCon longtycon, sub PPCore.ppTy ty])
      | ppSigExp(out, i, FCTSigExp(I, strid, sigexp1, sigexp2)) =
	    ppElem(out, i, "FCTSigExp", I,
		   [sub PPCore.ppStrId strid, sub ppSigExp sigexp1,
		    sub ppSigExp sigexp2])
      | ppSigExp(out, i, FCTSPECSigExp(I, spec, sigexp)) =
	    ppElem(out, i, "FCTSPECSigExp", I,
		   [sub ppSpec spec, sub ppSigExp sigexp])
      | ppSigExp(out, i, PARSigExp(I, sigexp)) =
	    ppElem(out, i, "PARSigExp", I,
		   [sub ppSigExp sigexp])

    and ppSigBind(out, i, SigBind(I, sigid, sigexp, sigbind_opt)) =
	    ppElem(out, i, "SigBind", I,
		   [sub ppSigId sigid, sub ppSigExp sigexp,
		    subo ppSigBind sigbind_opt])


    (* Specifications *)

    and ppSpec(out, i, VALSpec(I, valdesc)) =
	    ppElem(out, i, "VALSpec", I,
		   [sub ppValDesc valdesc])
      | ppSpec(out, i, TYPESpec(I, typdesc)) =
	    ppElem(out, i, "TYPESpec", I,
		   [sub ppTypDesc typdesc])
      | ppSpec(out, i, EQTYPESpec(I, typdesc)) =
	    ppElem(out, i, "EQTYPESpec", I,
		   [sub ppTypDesc typdesc])
      | ppSpec(out, i, DATATYPESpec(I, datdesc)) =
	    ppElem(out, i, "DATATYPESpec", I,
		   [sub ppDatDesc datdesc])
      | ppSpec(out, i, VIEWTYPESpec(I, tyvarseq, tycon, ty, condesc)) =
	    ppElem(out, i, "VIEWTYPESpec", I,
		   [sub PPCore.ppTyVarseq tyvarseq, sub PPCore.ppTyCon tycon,
		    sub PPCore.ppTy ty, sub ppConDesc condesc])
      | ppSpec(out, i, DATATYPE2Spec(I, tycon, longtycon)) =
	    ppElem(out, i, "DATATYPE2Spec", I,
		   [sub PPCore.ppTyCon tycon, sub PPCore.ppLongTyCon longtycon])
      | ppSpec(out, i, EXCEPTIONSpec(I, exdesc)) =
	    ppElem(out, i, "EXCEPTIONSpec", I,
		   [sub ppExDesc exdesc])
      | ppSpec(out, i, STRUCTURESpec(I, strdesc)) =
	    ppElem(out, i, "STRUCTURESpec", I,
		   [sub ppStrDesc strdesc])
      | ppSpec(out, i, SIGNATURESpec(I, sigdesc)) =
	    ppElem(out, i, "SIGNATURESpec", I,
		   [sub ppSigDesc sigdesc])
      | ppSpec(out, i, INCLUDESpec(I, sigexp)) =
	    ppElem(out, i, "INCLUDESpec", I,
		   [sub ppSigExp sigexp])
      | ppSpec(out, i, EMPTYSpec(I)) =
	    ppElem(out, i, "EMPTYSpec", I, [])
      | ppSpec(out, i, SEQSpec(I, spec1, spec2)) =
	    ppElem(out, i, "SEQSpec", I,
		   [sub ppSpec spec1, sub ppSpec spec2])
      | ppSpec(out, i, SHARINGTYPESpec(I, spec, longtycons)) =
	    ppElem(out, i, "SHARINGTYPESpec", I,
		   [sub ppSpec spec, subs PPCore.ppLongTyCon longtycons])
      | ppSpec(out, i, SHARINGSpec(I, spec, longstrids)) =
	    ppElem(out, i, "SHARINGSpec", I,
		   [sub ppSpec spec, subs PPCore.ppLongStrId longstrids])

    and ppValDesc(out, i, ValDesc(I, vid, ty, valdesc_opt)) =
	    ppElem(out, i, "ValDesc", I,
		   [sub PPCore.ppVId vid, sub PPCore.ppTy ty,
		    subo ppValDesc valdesc_opt])
    and ppTypDesc(out, i, TypDesc(I, tyvarseq, tycon, typdesc_opt)) =
	    ppElem(out, i, "TypDec", I,
		   [sub PPCore.ppTyVarseq tyvarseq, sub PPCore.ppTyCon tycon,
		    subo ppTypDesc typdesc_opt])
    and ppDatDesc(out, i, DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
	    ppElem(out, i, "DatDesc", I,
		   [sub PPCore.ppTyVarseq tyvarseq, sub PPCore.ppTyCon tycon,
		    sub ppConDesc condesc, subo ppDatDesc datdesc_opt])
    and ppConDesc(out, i, ConDesc(I, vid, ty_opt, condesc_opt)) =
	    ppElem(out, i, "ConDesc", I,
		   [sub PPCore.ppVId vid, subo PPCore.ppTy ty_opt,
		    subo ppConDesc condesc_opt])
    and ppExDesc(out, i, ExDesc(I, vid, ty_opt, exdesc_opt)) =
	    ppElem(out, i, "ExDesc", I,
		   [sub PPCore.ppVId vid, subo PPCore.ppTy ty_opt,
		    subo ppExDesc exdesc_opt])
    and ppStrDesc(out, i, StrDesc(I, strid, sigexp, strdesc_opt)) =
	    ppElem(out, i, "StrDesc", I,
		   [sub PPCore.ppStrId strid, sub ppSigExp sigexp,
		    subo ppStrDesc strdesc_opt])
    and ppSigDesc(out, i, SigDesc(I, sigid, sigexp, sigdesc_opt)) =
	    ppElem(out, i, "SigDesc", I,
		   [sub ppSigId sigid, sub ppSigExp sigexp,
		    subo ppSigDesc sigdesc_opt])


    (* Top-level declarations *)

    and ppTopDec(out, i, TopDec(I, dec)) =
	    ppElem(out, i, "TopDec", I,
		   [sub PPCore.ppDec dec])


    (* Tie recursive imports *)

    val _ = PPCore.PPModule.ppLongSigId := ppLongSigId
    val _ = PPCore.PPModule.ppStrExp := ppStrExp
    val _ = PPCore.PPModule.ppStrDec :=
            (fn(out, i, StrDec strdec) => ppStrDec(out, i, strdec)
              | _ => raise Fail "PPModule.ppStrDec: invalid declaration")
end;
