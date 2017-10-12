(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core derived forms
 *
 * Definition, Section 2.7 and Appendix A
 *
 * Notes:
 * - Two phrases named Fmatch and Fmrule have been added to factorize FvalBind.
 * - In Fvalbinds we do not enforce that all optional type annotations are
 *   syntactically identical (as the Definition enforces, although this seems
 *   to be a mistake).
 *)

signature DERIVED_FORMS_CORE =
sig
  (* Import *)

  type Lab     = SyntaxCore.Lab
  type VId     = SyntaxCore.VId
  type TyVar   = SyntaxCore.TyVar

  type Op      = SyntaxCore.Op
  type AtExp   = SyntaxCore.AtExp
  type AppExp  = SyntaxCore.AtExp list
  type InfExp  = SyntaxCore.Exp
  type Exp     = SyntaxCore.Exp
  type Match   = SyntaxCore.Match
  type Mrule   = SyntaxCore.Mrule
  type Dec     = SyntaxCore.Dec
  type ValBind = SyntaxCore.ValBind
  type TypBind = SyntaxCore.TypBind
  type DatBind = SyntaxCore.DatBind
  type AtPat   = SyntaxCore.AtPat
  type PatRow  = SyntaxCore.PatRow
  type Pat     = SyntaxCore.Pat
  type Ty      = SyntaxCore.Ty
  type 'a seq  = 'a SyntaxCore.seq


  (* Types *)

  type FvalBind' = SyntaxCore.ValBind'
  type Fmatch'
  type Fmrule'
  type FvalBind  = ValBind
  type Fmatch    = (Fmatch', unit) Annotation.phrase
  type Fmrule    = (Fmrule', unit) Annotation.phrase


  (* Expressions [Figure 15] *)

  val UNITAtExp   : SyntaxCore.AtExp'
  val TUPLEAtExp  : Exp list -> SyntaxCore.AtExp'
  val HASHAtExp   : Lab -> SyntaxCore.AtExp'
  val CASEExp     : Exp * Match -> SyntaxCore.Exp'
  val IFExp       : Exp * Exp * Exp -> SyntaxCore.Exp'
  val ANDALSOExp  : Exp * Exp -> SyntaxCore.Exp'
  val ORELSEExp   : Exp * Exp -> SyntaxCore.Exp'
  val SEQAtExp    : Exp list -> SyntaxCore.AtExp'
  val LETSEQAtExp : Dec * Exp list -> SyntaxCore.AtExp'
  val WHILEExp    : Exp * Exp -> SyntaxCore.Exp'
  val LISTAtExp   : Exp list -> SyntaxCore.AtExp'

  (* Patterns [Figure 16] *)

  val UNITAtPat  : SyntaxCore.AtPat'
  val TUPLEAtPat : Pat list -> SyntaxCore.AtPat'
  val LISTAtPat  : Pat list -> SyntaxCore.AtPat'

  val IDPatRow :
      VId * Ty option * Pat option * PatRow option -> SyntaxCore.PatRow'

  (* Types [Figure 16] *)

  val TUPLETy : Ty list -> SyntaxCore.Ty'

  (* Function-value bindings [Figure 17] *)

  val FvalBind : Fmatch * FvalBind option -> FvalBind'
  val Fmatch   : Fmrule * Fmatch option -> Fmatch'
  val Fmrule   : Op option * VId * AtPat list * Ty option * Exp -> Fmrule'

  (* Declarations [Figure 17] *)

  val FUNDec              : TyVar seq * FvalBind -> SyntaxCore.Dec'
  val DATATYPEWITHTYPEDec : DatBind * TypBind option -> SyntaxCore.Dec'
  val ABSTYPEWITHTYPEDec  : DatBind * TypBind option * Dec -> SyntaxCore.Dec'
end;
