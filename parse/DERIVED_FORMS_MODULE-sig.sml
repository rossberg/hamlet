(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML modules derived forms
 *
 * Definition, Appendix A
 *
 * Notes:
 * - A phrase named SynDesc has been added to factorize type synonym
 *   specifications.
 * - Similarly, a phrase named TyReaDesc has been added to factorize type
 *   realisation signature expressions.
 * - The structure sharing derived form is omitted since it cannot be resolved
 *   syntactically -- it has been moved to the bare grammar.
 *)

signature DERIVED_FORMS_MODULE =
sig
  (* Import *)

  type VId       = SyntaxCore.VId
  type TyCon     = SyntaxCore.TyCon
  type StrId     = SyntaxCore.StrId
  type SigId     = SyntaxModule.SigId
  type FunId     = SyntaxModule.FunId
  type longTyCon = SyntaxCore.longTyCon
  type TyVar     = SyntaxCore.TyVar

  type Ty        = SyntaxCore.Ty
  type 'a seq    = 'a SyntaxCore.seq

  type StrExp    = SyntaxModule.StrExp
  type StrDec    = SyntaxModule.StrDec
  type StrBind   = SyntaxModule.StrBind
  type SigExp    = SyntaxModule.SigExp
  type Spec      = SyntaxModule.Spec
  type FunBind   = SyntaxModule.FunBind


  (* Types *)

  type SynDesc'
  type TyReaDesc'
  type SynDesc    = (SynDesc', unit) Annotation.phrase
  type TyReaDesc  = (TyReaDesc', unit) Annotation.phrase


  (* Structure Bindings [Figure 18] *)

  val PLAINStrBind :
      StrId * StrExp * StrBind option -> SyntaxModule.StrBind'
  val COLONStrBind :
      StrId * SigExp * StrExp * StrBind option -> SyntaxModule.StrBind'
  val SEALStrBind :
      StrId * SigExp * StrExp * StrBind option -> SyntaxModule.StrBind'

  (* Structure Expressions [Figure 18] *)

  val APPDECStrExp : FunId * StrDec -> SyntaxModule.StrExp'

  (* Functor Bindings [Figure 18] *)

  val PLAINFunBind :
      FunId * StrId * SigExp * StrExp * FunBind option -> SyntaxModule.FunBind'
  val COLONFunBind :
      FunId * StrId * SigExp * SigExp * StrExp * FunBind option ->
        SyntaxModule.FunBind'
  val SEALFunBind :
      FunId * StrId * SigExp * SigExp * StrExp * FunBind option ->
        SyntaxModule.FunBind'
  val SPECFunBind :
      FunId * Spec * StrExp * FunBind option -> SyntaxModule.FunBind'
  val COLONSPECFunBind :
      FunId * Spec * SigExp * StrExp * FunBind option -> SyntaxModule.FunBind'
  val SEALSPECFunBind :
      FunId * Spec * SigExp * StrExp * FunBind option -> SyntaxModule.FunBind'

  (* Specifications [Figure 19] *)

  val SYNSpec          : SynDesc -> SyntaxModule.Spec'
  val INCLUDEMULTISpec : SigId list -> SyntaxModule.Spec'

  val SynDesc          : TyVar seq * TyCon * Ty * SynDesc option -> SynDesc'

  (* Signature Expressions [Figure 19] *)

  val WHERETYPEMULTISigExp : SigExp * TyReaDesc -> SyntaxModule.SigExp'

  val TyReaDesc : TyVar seq * longTyCon * Ty * TyReaDesc option -> TyReaDesc'
end;
