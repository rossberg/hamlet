(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML modules derived forms
 *
 * Definition, Appendix A
 *
 * Notes: see DERIVED_FORMS_MODULE-sig.sml
 *)

structure DerivedFormsModule :> DERIVED_FORMS_MODULE =
struct
  (* Import *)

  open SyntaxCore
  open SyntaxModule
  open AnnotationModule


  (* Types *)

  type SynDesc'   = (TyVar seq * TyCon * Ty) list
  type TyReaDesc' = (TyVar seq * longTyCon * Ty) list
  type SynDesc    = (SynDesc', unit) phrase
  type TyReaDesc  = (TyReaDesc', unit) phrase


  (* Structure Bindings [Figure 18] *)

  val PLAINStrBind = StrBind

  fun COLONStrBind(strid, sigexp, strexp, strbind_opt) =
        StrBind(strid, COLONStrExp(strexp, sigexp)@@over(sigexp, strexp),
          strbind_opt)

  fun SEALStrBind(strid, sigexp, strexp, strbind_opt) =
        StrBind(strid, SEALStrExp(strexp, sigexp)@@over(sigexp, strexp),
          strbind_opt)


  (* Structure Expressions [Figure 18] *)

  fun APPDECStrExp(funid, strdec) =
        APPStrExp(funid, STRUCTStrExp(strdec)@@at(strdec))


  (* Functor Bindings [Figure 18] *)

  val PLAINFunBind = FunBind

  fun COLONFunBind(funid, strid, sigexp, sigexp', strexp, funbind_opt)=
      let
        val colonExp = COLONStrExp(strexp, sigexp')@@over(sigexp', strexp)
      in
        FunBind(funid, strid, sigexp, colonExp, funbind_opt)
      end

  fun SEALFunBind(funid, strid, sigexp, sigexp', strexp, funbind_opt) =
      let
        val sealExp = SEALStrExp(strexp, sigexp')@@over(sigexp', strexp)
      in
        FunBind(funid, strid, sigexp, sealExp, funbind_opt)
      end

  fun SPECFunBind(funid, spec, strexp, funbind_opt) =
      let
        val strid   = StrId.invent()
        val sigexp  = SIGSigExp(spec)@@at(spec)
        val openDec = OPENDec[LongStrId.fromId(strid)@@at(spec)]@@at(spec)
        val strdec  = DECStrDec(openDec)@@at(openDec)
        val letExp  = LETStrExp(strdec, strexp)@@over(spec, strexp)
      in
        FunBind(funid, strid@@at(spec), sigexp, letExp, funbind_opt)
      end

  fun COLONSPECFunBind(funid, spec, sigexp, strexp, funbind_opt) =
      let
        val colonExp = COLONStrExp(strexp, sigexp)@@over(sigexp, strexp)
      in
        SPECFunBind(funid, spec, colonExp, funbind_opt)
      end

  fun SEALSPECFunBind(funid, spec, sigexp, strexp, funbind_opt) =
      let
        val sealExp = SEALStrExp(strexp, sigexp)@@over(sigexp, strexp)
      in
        SPECFunBind(funid, spec, sealExp, funbind_opt)
      end


  (* Specifications [Figure 19] *)

  fun SYNSpec(syndescs@@_) =
      let
        val (lastTyvarseq, _, lastTy) = List.hd(List.rev syndescs)

        fun toSpec[] = EMPTYSpec@@over(lastTyvarseq, lastTy)
          | toSpec((tyvarseq, tycon, ty)::syndescs') =
            let
              val longtycon = LongTyCon.fromId(syntax tycon)@@at(tycon)
              val typdesc =
                  TypDesc(tyvarseq, tycon, NONE)@@over(tyvarseq, tycon)
              val typeSpec = TYPESpec(typdesc)@@at(typdesc)
              val sigexp  = SIGSigExp(typeSpec)@@at(typeSpec)
              val sigexp' =
                  WHERETYPESigExp(sigexp, tyvarseq, longtycon, ty)
                    @@over(sigexp, ty)
              val spec  = INCLUDESpec(sigexp')@@at(sigexp')
              val spec' = toSpec(syndescs')
            in
              SEQSpec(spec, spec')@@over(spec, spec')
            end

            val spec'@@_ = toSpec syndescs
        in
            spec'
        end

  fun INCLUDEMULTISpec[] = EMPTYSpec
    | INCLUDEMULTISpec(sigid::sigids') =
      let
        val sigexp = IDSigExp(sigid)@@at(sigid)
        val spec   = INCLUDESpec(sigexp)@@at(sigexp)
        val spec'  = INCLUDEMULTISpec(sigids')@@overAll(sigids')
      in
        SEQSpec(spec, spec')
      end

  fun SynDesc(tyvarseq, tycon, ty, NONE) =
        (tyvarseq, tycon, ty) :: []
    | SynDesc(tyvarseq, tycon, ty, SOME(syndesc'@@_)) =
        (tyvarseq, tycon, ty) :: syndesc'


  (* Signature Expressions [Figure 19] *)

  fun WHERETYPEMULTISigExp(sigexp'@@_, []@@_) = sigexp'
    | WHERETYPEMULTISigExp(sigexp, (tyvarseq, longtycon, ty)::reas'@@A) =
      let
        val sigexp' =
            WHERETYPESigExp(sigexp, tyvarseq, longtycon, ty)@@over(sigexp, ty)
      in
        WHERETYPEMULTISigExp(sigexp', reas'@@A)
      end

  fun TyReaDesc(tyvarseq, longtycon, ty, NONE) =
        (tyvarseq, longtycon, ty) :: []
    | TyReaDesc(tyvarseq, longtycon, ty, SOME(tyreadesc'@@_)) =
        (tyvarseq, longtycon, ty) :: tyreadesc'
end;
