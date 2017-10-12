(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules derived forms
 *
 * Definition, Appendix A
 * + RFC: Fixed manifest type specifications
 * + RFC: Views
 * + RFC: Withtype in signatures
 * + RFC: Abolish sequenced type realisations
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: Local modules
 *
 * Notes:
 * - A phrase named SynDesc has been added to factorize type synonym
 *   specifications.
 * - The structure sharing derived form is missing since it cannot be resolved
 *   syntactically. It has been moved to the bare grammar.
 * - Likewise, the functor signature derived form with a spec argument.
 *)

signature DERIVED_FORMS_MODULE =
sig
    (* Import *)

    type Info       = GrammarModule.Info

    type VId        = GrammarCore.VId
    type TyCon      = GrammarCore.TyCon
    type StrId      = GrammarCore.StrId
    type SigId      = GrammarModule.SigId
    type longTyCon  = GrammarCore.longTyCon
    type longStrId  = GrammarCore.longStrId
    type longSigId  = GrammarModule.longSigId

    type Ty         = GrammarCore.Ty
    type TyVarseq   = GrammarCore.TyVarseq
    type TypBind    = GrammarCore.TypBind
    type Dec        = GrammarCore.Dec

    type AtStrExp   = GrammarModule.StrExp
    type AppStrExp  = GrammarModule.StrExp
    type StrExp     = GrammarModule.StrExp
    type StrDec     = GrammarModule.StrDec
    type StrBind    = GrammarModule.StrBind
    type AtSigExp   = GrammarModule.SigExp
    type SigExp     = GrammarModule.SigExp
    type Spec       = GrammarModule.Spec
    type SynDesc    = GrammarModule.TypDesc * (SigExp -> SigExp)
    type DatDesc    = GrammarModule.DatDesc
    type FunBind    = GrammarModule.StrBind
    type FunDesc    = GrammarModule.StrDesc


    (* Structure Bindings [Figure 18] *)

    val TRANSStrBind :		Info * StrId * SigExp option * StrExp
				     * StrBind option -> StrBind
    val SEALStrBind :		Info * StrId * SigExp * StrExp
				     * StrBind option -> StrBind

    (* Structure Expressions [Figure 18; RFC: Higher-order functors;
     *                                   RFC: Local modules] *)

    val DECStrExp :		Info * Dec -> StrExp
    val FCTSPECStrExp :		Info * Spec * StrExp -> StrExp

    (* Structure Declarations [Figure 18; RFC: Higher-order functors] *)

    val FUNCTORStrDec :		Info * FunBind -> StrDec

    (* Functor Arguments [Figure 18; Figure 23c; RFC: Higher-order functors] *)

    datatype FunArg =
	  COLONFunArg of Info * StrId * SigExp
	| SPECFunArg  of Info * Spec

    (* Functor Bindings [Figure 18; RFC: Higher-order functors] *)

    val TRANSFunBind :		Info * StrId * FunArg list * SigExp option
				     * StrExp * FunBind option -> FunBind
    val SEALFunBind :		Info * StrId * FunArg list * SigExp
				     * StrExp * FunBind option -> FunBind

    (* Functor Descriptions [Figure 19; RFC: Higher-order functors] *)

    val FunDesc :		Info * StrId * FunArg list * SigExp
				     * FunDesc option -> FunDesc

    (* Signature Expressions [Figure 19;
     *                        RFC: Abolish sequenced type relisations;
     *                        RFC: Higher-order functors] *)

    val SPECSigExp :		Info * Spec -> SigExp
    val ARROWSigExp :		Info * SigExp * SigExp -> SigExp
    
    (* Specifications [Figure 19; RFC: Views; RFC: Nested signatures;
     *                            RFC: Higher-order functors] *)

    val SYNSpec :		Info * SynDesc -> Spec
    val DATATYPESpec :	 	Info * DatDesc * TypBind option -> Spec
    val VIEWTYPE2Spec :	 	Info * TyCon * longTyCon -> Spec
    val FUNCTORSpec :		Info * FunDesc -> Spec
    val INCLUDEMULTISpec :	Info * longSigId list -> Spec

    val SynDesc :		Info * TyVarseq * TyCon * Ty
				     * SynDesc option -> SynDesc
end;
