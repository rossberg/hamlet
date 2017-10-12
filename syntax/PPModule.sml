(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Printer for abstract module syntax
 *)

structure PPModule : PP_MODULE =
struct
  (* Import *)

  open SyntaxModule
  open Annotation
  open PPSyntax


  (* Identifiers *)

  fun ppSigId(out, i, sigid@@_) = ppAtom(out, i, "SigId", SigId.toString sigid)
  fun ppFunId(out, i, funid@@_) = ppAtom(out, i, "FunId", FunId.toString funid)


  (* Structures *)

  fun ppStrExp(out, i, STRUCTStrExp(strdec)@@A) =
        ppElem(out, i, "STRUCTStrExp", A, [sub ppStrDec strdec])
    | ppStrExp(out, i, IDStrExp(longstrid)@@A) =
        ppElem(out, i, "IDStrExp", A, [sub PPCore.ppLongStrId longstrid])
    | ppStrExp(out, i, COLONStrExp(strexp, sigexp)@@A) =
        ppElem(out, i, "COLONStrExp", A,
          [sub ppStrExp strexp, sub ppSigExp sigexp])
    | ppStrExp(out, i, SEALStrExp(strexp, sigexp)@@A) =
        ppElem(out, i, "SEALStrExp", A,
          [sub ppStrExp strexp, sub ppSigExp sigexp])
    | ppStrExp(out, i, APPStrExp(funid, strexp)@@A) =
        ppElem(out, i, "APPStrExp", A, [sub ppFunId funid, sub ppStrExp strexp])
    | ppStrExp(out, i, LETStrExp(strdec, strexp)@@A) =
        ppElem(out, i, "LETStrExp", A,
          [sub ppStrDec strdec, sub ppStrExp strexp])

  and ppStrDec(out, i, DECStrDec(dec)@@A) =
        ppElem(out, i, "DECStrDec", A, [sub PPCore.ppDec dec])
    | ppStrDec(out, i, STRUCTUREStrDec(strbind)@@A) =
        ppElem(out, i, "STRUCTUREStrDec", A, [sub ppStrBind strbind])
    | ppStrDec(out, i, LOCALStrDec(strdec1, strdec2)@@A) =
        ppElem(out, i, "LOCALStrDec", A,
          [sub ppStrDec strdec1, sub ppStrDec strdec2])
    | ppStrDec(out, i, EMPTYStrDec@@A) =
        ppElem(out, i, "EMPTYStrDec", A, [])
    | ppStrDec(out, i, SEQStrDec(strdec1, strdec2)@@A) =
        ppElem(out, i, "SEQStrDec", A,
          [sub ppStrDec strdec1, sub ppStrDec strdec2])

  and ppStrBind(out, i, StrBind(strid, strexp, strbind_opt)@@A) =
        ppElem(out, i, "StrBind", A,
          [sub PPCore.ppStrId strid, sub ppStrExp strexp,
            subo ppStrBind strbind_opt])


  (* Signatures *)

  and ppSigExp(out, i, SIGSigExp(spec)@@A) =
        ppElem(out, i, "SIGSigExp", A, [sub ppSpec spec])
    | ppSigExp(out, i, IDSigExp(sigid)@@A) =
        ppElem(out, i, "IDSigExp", A, [sub ppSigId sigid])
    | ppSigExp(out, i, WHERETYPESigExp(sigexp, tyvarseq, longtycon, ty)@@A) =
        ppElem(out, i, "WHERETYPESigExp", A,
          [sub ppSigExp sigexp, sub PPCore.ppTyVarseq tyvarseq,
            sub PPCore.ppLongTyCon longtycon, sub PPCore.ppTy ty])

  and ppSigDec(out, i, SigDec(sigbind)@@A) =
        ppElem(out, i, "SigDec", A, [sub ppSigBind sigbind])

  and ppSigBind(out, i, SigBind(sigid, sigexp, sigbind_opt)@@A) =
        ppElem(out, i, "SigBind", A,
          [sub ppSigId sigid, sub ppSigExp sigexp, subo ppSigBind sigbind_opt])


  (* Specifications *)

  and ppSpec(out, i, VALSpec(valdesc)@@A) =
        ppElem(out, i, "VALSpec", A, [sub ppValDesc valdesc])
    | ppSpec(out, i, TYPESpec(typdesc)@@A) =
        ppElem(out, i, "TYPESpec", A, [sub ppTypDesc typdesc])
    | ppSpec(out, i, EQTYPESpec(typdesc)@@A) =
        ppElem(out, i, "EQTYPESpec", A, [sub ppTypDesc typdesc])
    | ppSpec(out, i, DATATYPESpec(datdesc)@@A) =
        ppElem(out, i, "DATATYPESpec", A, [sub ppDatDesc datdesc])
    | ppSpec(out, i, DATATYPE2Spec(tycon, longtycon)@@A) =
        ppElem(out, i, "DATATYPE2Spec", A,
          [sub PPCore.ppTyCon tycon, sub PPCore.ppLongTyCon longtycon])
    | ppSpec(out, i, EXCEPTIONSpec(exdesc)@@A) =
        ppElem(out, i, "EXCEPTIONSpec", A, [sub ppExDesc exdesc])
    | ppSpec(out, i, STRUCTURESpec(strdesc)@@A) =
        ppElem(out, i, "STRUCTURESpec", A, [sub ppStrDesc strdesc])
    | ppSpec(out, i, INCLUDESpec(sigexp)@@A) =
        ppElem(out, i, "INCLUDESpec", A, [sub ppSigExp sigexp])
    | ppSpec(out, i, EMPTYSpec@@A) =
        ppElem(out, i, "EMPTYSpec", A, [])
    | ppSpec(out, i, SEQSpec(spec1, spec2)@@A) =
        ppElem(out, i, "SEQSpec", A, [sub ppSpec spec1, sub ppSpec spec2])
    | ppSpec(out, i, SHARINGTYPESpec(spec, longtycons)@@A) =
        ppElem(out, i, "SHARINGTYPESpec", A,
          [sub ppSpec spec, subs PPCore.ppLongTyCon longtycons])
    | ppSpec(out, i, SHARINGSpec(spec, longstrids)@@A) =
        ppElem(out, i, "SHARINGSpec", A,
          [sub ppSpec spec, subs PPCore.ppLongStrId longstrids])

  and ppValDesc(out, i, ValDesc(vid, ty, valdesc_opt)@@A) =
        ppElem(out, i, "ValDesc", A,
          [sub PPCore.ppVId vid, sub PPCore.ppTy ty,
            subo ppValDesc valdesc_opt])

  and ppTypDesc(out, i, TypDesc(tyvarseq, tycon, typdesc_opt)@@A) =
        ppElem(out, i, "TypDec", A,
          [sub PPCore.ppTyVarseq tyvarseq, sub PPCore.ppTyCon tycon,
            subo ppTypDesc typdesc_opt])

  and ppDatDesc(out, i, DatDesc(tyvarseq, tycon, condesc, datdesc_opt)@@A) =
        ppElem(out, i, "DatDesc", A,
          [sub PPCore.ppTyVarseq tyvarseq, sub PPCore.ppTyCon tycon,
            sub ppConDesc condesc, subo ppDatDesc datdesc_opt])

  and ppConDesc(out, i, ConDesc(vid, ty_opt, condesc_opt)@@A) =
        ppElem(out, i, "ConDesc", A,
          [sub PPCore.ppVId vid, subo PPCore.ppTy ty_opt,
            subo ppConDesc condesc_opt])

  and ppExDesc(out, i, ExDesc(vid, ty_opt, exdesc_opt)@@A) =
        ppElem(out, i, "ExDesc", A,
          [sub PPCore.ppVId vid, subo PPCore.ppTy ty_opt,
            subo ppExDesc exdesc_opt])

  and ppStrDesc(out, i, StrDesc(strid, sigexp, strdesc_opt)@@A) =
        ppElem(out, i, "StrDesc", A,
          [sub PPCore.ppStrId strid, sub ppSigExp sigexp,
            subo ppStrDesc strdesc_opt])


  (* Functors *)

  and ppFunDec(out, i, FunDec(funbind)@@A) =
        ppElem(out, i, "FunDec", A, [sub ppFunBind funbind])

  and ppFunBind(out, i,
        FunBind(funid, strid, sigexp, strexp, funbind_opt)@@A
      ) =
        ppElem(out, i, "FunBind", A,
          [sub ppFunId funid, sub PPCore.ppStrId strid,
            sub ppSigExp sigexp, sub ppStrExp strexp,
              subo ppFunBind funbind_opt])


  (* Top-level declarations *)

  and ppTopDec(out, i, STRDECTopDec(strdec, topdec_opt)@@A) =
        ppElem(out, i, "STRDECTopDec", A,
          [sub ppStrDec strdec, subo ppTopDec topdec_opt])
    | ppTopDec(out, i, SIGDECTopDec(sigdec, topdec_opt)@@A) =
        ppElem(out, i, "SIGDECTopDec", A,
          [sub ppSigDec sigdec, subo ppTopDec topdec_opt])
    | ppTopDec(out, i, FUNDECTopDec(fundec, topdec_opt)@@A) =
        ppElem(out, i, "FUNDECTopDec", A,
          [sub ppFunDec fundec, subo ppTopDec topdec_opt])
end;
