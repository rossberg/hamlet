(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML abstract core syntax
 *
 * Definition, Section 2.8
 *
 * Note:
 *   This is the syntax used in the inference rules for the core [Definition,
 *   Sections 4.10 and 6.7]. It omits almost anything having to do with infixed
 *   identifiers:
 *   - fixity directives
 *   - infixed application
 *   - infixed value construction
 *   However, 'op' prefixes are kept, since they are required for rebuilding the
 *   syntax tree during fixity resolution. Optional semicolons are also omitted.
 *)

functor SyntaxCoreFn(
  type SCon_attr
  type Lab_attr
  type VId_attr
  type TyCon_attr
  type TyVar_attr
  type StrId_attr
  type longVId_attr
  type longTyCon_attr
  type longStrId_attr

  type AtExp_attr
  type ExpRow_attr
  type Exp_attr
  type Match_attr
  type Mrule_attr
  type Dec_attr
  type ValBind_attr
  type TypBind_attr
  type DatBind_attr
  type ConBind_attr
  type ExBind_attr
  type AtPat_attr
  type PatRow_attr
  type Pat_attr
  type Ty_attr
  type TyRow_attr
  type 'a seq_attr
) =
struct
  (* Import *)

  type ('a, 'b) phrase = ('a, 'b) Annotation.phrase

  type SCon      = (SCon.SCon, SCon_attr) phrase
  type Lab       = (Lab.Lab, Lab_attr) phrase
  type VId       = (VId.Id, VId_attr) phrase
  type TyCon     = (TyCon.Id, TyCon_attr) phrase
  type TyVar     = (TyVar.TyVar, TyVar_attr) phrase
  type StrId     = (StrId.Id, StrId_attr) phrase
  type longVId   = (LongVId.longId, longVId_attr) phrase
  type longTyCon = (LongTyCon.longId, longTyCon_attr) phrase
  type longStrId = (LongStrId.longId, longStrId_attr) phrase


  (* Optional keyword `op' *)

  datatype Op = Op


  (* Expressions [Figures 2 and 4] *)

  and AtExp' =
      SCONAtExp   of SCon
    | IDAtExp     of Op option * longVId
    | RECORDAtExp of ExpRow option
    | LETAtExp    of Dec * Exp
    | PARAtExp    of Exp

  and ExpRow' =
      ExpRow      of Lab * Exp * ExpRow option

  and Exp' =
      ATExp       of AtExp
    | APPExp      of Exp * AtExp
    | COLONExp    of Exp * Ty
    | HANDLEExp   of Exp * Match
    | RAISEExp    of Exp
    | FNExp       of Match


  (* Matches [Figures 2 and 4] *)

  and Match' =
      Match of Mrule * Match option

  and Mrule' =
      Mrule of Pat * Exp


  (* Declarations [Figures 2 and 4] *)

  and Dec' =
      VALDec       of TyVar seq * ValBind
    | TYPEDec      of TypBind
    | DATATYPEDec  of DatBind
    | DATATYPE2Dec of TyCon * longTyCon
    | ABSTYPEDec   of DatBind * Dec
    | EXCEPTIONDec of ExBind
    | LOCALDec     of Dec * Dec
    | OPENDec      of longStrId list
    | EMPTYDec
    | SEQDec       of Dec * Dec


  (* Bindings [Figures 2 and 4] *)

  and ValBind' =
      PLAINValBind of Pat * Exp * ValBind option
    | RECValBind   of ValBind

  and TypBind' =
      TypBind of TyVar seq * TyCon * Ty * TypBind option

  and DatBind' =
      DatBind of TyVar seq * TyCon * ConBind * DatBind option

  and ConBind' =
      ConBind of Op option * VId * Ty option * ConBind option

  and ExBind' =
      NEWExBind   of Op option * VId * Ty option * ExBind option
    | EQUALExBind of Op option * VId * Op option * longVId * ExBind option


  (* Patterns [Figures 2 and 3] *)

  and AtPat' =
      WILDCARDAtPat
    | SCONAtPat   of SCon
    | IDAtPat     of Op option * longVId
    | RECORDAtPat of PatRow option
    | PARAtPat    of Pat

  and PatRow' =
      DOTSPatRow
    | FIELDPatRow of Lab * Pat * PatRow option

  and Pat' =
      ATPat       of AtPat
    | CONPat      of Op option * longVId * AtPat
    | COLONPat    of Pat * Ty
    | ASPat       of Op option * VId * Ty option * Pat


  (* Type expressions [Figures 2 and 3] *)

  and Ty' =
      VARTy    of TyVar
    | RECORDTy of TyRow option
    | CONTy    of Ty seq * longTyCon
    | ARROWTy  of Ty * Ty
    | PARTy    of Ty

  and TyRow' =
      TyRow    of Lab * Ty * TyRow option


  (* Sequences [Section 2.8] *)

  and 'a seq' =
      Seq of 'a list


  (* Annotated syntax *)

  withtype
      AtExp   = (AtExp', AtExp_attr) phrase
  and ExpRow  = (ExpRow', ExpRow_attr) phrase
  and Exp     = (Exp', Exp_attr) phrase
  and Match   = (Match', Match_attr) phrase
  and Mrule   = (Mrule', Mrule_attr) phrase
  and Dec     = (Dec', Dec_attr) phrase
  and ValBind = (ValBind', ValBind_attr) phrase
  and TypBind = (TypBind', TypBind_attr) phrase
  and DatBind = (DatBind', DatBind_attr) phrase
  and ConBind = (ConBind', ConBind_attr) phrase
  and ExBind  = (ExBind', ExBind_attr) phrase
  and AtPat   = (AtPat', AtPat_attr) phrase
  and PatRow  = (PatRow', PatRow_attr) phrase
  and Pat     = (Pat', Pat_attr) phrase
  and Ty      = (Ty', Ty_attr) phrase
  and TyRow   = (TyRow', TyRow_attr) phrase
  and 'a seq  = ('a seq', 'a seq_attr) phrase
end;
