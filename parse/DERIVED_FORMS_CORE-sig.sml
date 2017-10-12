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
 * + RFC: Pattern guards
 * + RFC: Transformation patterns
 * + RFC: Views
 * + RFC: Optional else branch
 * + RFC: Do declarations
 * + RFC: Withtype in signatures
 * + RFC: Local modules
 * + RFC: First-class modules
 *)

signature DERIVED_FORMS_CORE =
sig
    (* Import *)

    type Info      = GrammarCore.Info

    type Lab       = GrammarCore.Lab
    type VId       = GrammarCore.VId
    type TyCon     = GrammarCore.TyCon
    type longTyCon = GrammarCore.longTyCon

    type Op        = GrammarCore.Op
    type AtExp     = GrammarCore.AtExp
    type ExpRow    = GrammarCore.ExpRow
    type AppExp    = GrammarCore.AtExp list
    type InfExp    = GrammarCore.Exp
    type Exp       = GrammarCore.Exp
    type Match     = GrammarCore.Match
    type Mrule     = GrammarCore.Mrule
    type Dec       = GrammarCore.Dec
    type ValBind   = GrammarCore.ValBind
    type FvalBind  = GrammarCore.ValBind
    type Fmatch    = GrammarCore.Match * VId * int
    type Fmrule    = GrammarCore.Mrule * VId * int
    type TypBind   = GrammarCore.TypBind
    type DatBind   = GrammarCore.DatBind
    type AtPat     = GrammarCore.AtPat
    type PatRow    = GrammarCore.PatRow
    type AppPat    = GrammarCore.AtPat list
    type InfPat    = GrammarCore.Pat
    type Pat       = GrammarCore.Pat
    type Ty        = GrammarCore.Ty
    type TyRow     = GrammarCore.TyRow
    type TyVarseq  = GrammarCore.TyVarseq

    type StrExp    = GrammarModule.StrExp
    type AtStrExp  = GrammarModule.StrExp
    type AtSigExp  = GrammarModule.SigExp


    (* Expressions [Figure 15; RFC: Optional else branch;
     *                         RFC: Record punning; RFC: Record extension;
     *                         RFC: Record update; RFC: First-class modules] *)

    val UNITAtExp :	Info					-> AtExp
    val TUPLEAtExp :	Info * Exp list				-> AtExp
    val UPDATEAtExp :	Info * AtExp * ExpRow			-> AtExp
    val HASHAtExp :	Info * Lab				-> AtExp
    val PACKExp :	Info * AtStrExp * AtSigExp		-> Exp
    val CASEExp :	Info * Exp * Match			-> Exp
    val IFExp :		Info * Exp * Exp * Exp option		-> Exp
    val ANDALSOExp :	Info * Exp * Exp			-> Exp
    val ORELSEExp :	Info * Exp * Exp			-> Exp
    val SEQAtExp :	Info * Exp list				-> AtExp
    val LETAtExp :	Info * Dec * Exp list			-> AtExp
    val WHILEExp :	Info * Exp * Exp			-> Exp
    val LISTAtExp :	Info * Exp list				-> AtExp

    val IDExpRow :	Info * VId * Ty option * ExpRow option	-> ExpRow
    val DOTSExpRow :	Info * Exp * ExpRow option		-> ExpRow

    (* Patterns [Figure 16; RFC: Record extension; RFC: Pattern guards;
     *                      RFC: Transformation patterns] *)

    val UNITAtPat :	Info					-> AtPat
    val TUPLEAtPat :	Info * Pat list				-> AtPat
    val LISTAtPat :	Info * Pat list				-> AtPat
    val QUESTAtPat :	Info * AtExp				-> AtPat

    val DOTSPatRow:	Info * Pat option * PatRow option	-> PatRow
    val IDPatRow :	Info * VId * Ty option * Pat option * PatRow option
								-> PatRow

    val QUESTCONPat :	Info * AtExp * AtPat			-> Pat
    val IFPat :		Info * Pat * Exp			-> Pat

    (* Types [Figure 16; RFC: Record extension] *)

    val TUPLETy :	Info * Ty list				-> Ty

    val DOTSTyRow :	Info * Ty * TyRow option		-> TyRow

    (* Function-value bindings [Figure 17; RFC: Syntax fixes;
     *                                     RFC: Pattern guards] *)

    val FvalBind :	Info * Fmatch * FvalBind option		-> FvalBind
    val Fmatch :	Info * Fmrule * Fmatch option		-> Fmatch
    val Fmrule :	Info * Op * VId * AtPat list * Ty option
			     * AtExp option * Exp		-> Fmrule

    (* Declarations [Figure 17; RFC: Do declarations; RFC: Views;
			        RFC: Local modules] *)

    val DODec :		Info * Exp				-> Dec
    val FUNDec :	Info * TyVarseq * FvalBind		-> Dec
    val DATATYPEDec :	Info * DatBind * TypBind option		-> Dec
    val VIEWTYPE2Dec :	Info * TyCon * longTyCon		-> Dec
    val ABSTYPEDec :	Info * DatBind * TypBind option * Dec	-> Dec
    val OPENDec :	Info * StrExp				-> Dec

    val findTyCon :	TyCon * TypBind -> (TyVarseq * Ty) option 
    val rewriteTy :	TypBind -> Ty -> Ty
end;
