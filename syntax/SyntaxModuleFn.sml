(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML abstract module syntax
 *
 * Definition, Section 3.4
 *
 * Notes:
 *   This is the syntax used in the inference rules for modules [Definition,
 *   Sections 5.7 and 7.3]. Optional semicolons are omitted.
 *   On the other hand, we added the structure sharing derived form [Definition,
 *   Appendix A], since it cannot actually be derived purely syntactically.
 *)

functor SyntaxModuleFn(
  structure Core :
  sig
    type VId
    type TyCon
    type TyVar
    type StrId
    type longTyCon
    type longStrId

    type Dec
    type Ty
    type 'a seq
  end

  type SigId_attr
  type FunId_attr

  type StrExp_attr
  type StrDec_attr
  type StrBind_attr
  type SigExp_attr
  type SigDec_attr
  type SigBind_attr
  type Spec_attr
  type ValDesc_attr
  type TypDesc_attr
  type DatDesc_attr
  type ConDesc_attr
  type ExDesc_attr
  type StrDesc_attr
  type FunDec_attr
  type FunBind_attr
  type TopDec_attr
) =
struct
  (* Import *)

  open Core

  type ('a, 'b) phrase = ('a, 'b) Annotation.phrase

  type SigId = (SigId.Id, SigId_attr) phrase
  type FunId = (FunId.Id, FunId_attr) phrase


  (* Structures [Figures 5 and 6] *)

  datatype StrExp' =
      STRUCTStrExp    of StrDec
    | IDStrExp        of longStrId
    | COLONStrExp     of StrExp * SigExp
    | SEALStrExp      of StrExp * SigExp
    | APPStrExp       of FunId * StrExp
    | LETStrExp       of StrDec * StrExp

  and StrDec' =
      DECStrDec       of Dec
    | STRUCTUREStrDec of StrBind
    | LOCALStrDec     of StrDec * StrDec
    | EMPTYStrDec
    | SEQStrDec       of StrDec * StrDec

  and StrBind' =
      StrBind of StrId * StrExp * StrBind option


  (* Signatures [Figures 5 and 6] *)

  and SigExp' =
      SIGSigExp       of Spec
    | IDSigExp        of SigId
    | WHERETYPESigExp of SigExp * TyVar seq * longTyCon * Ty

  and SigDec' =
      SigDec of SigBind

  and SigBind' =
      SigBind of SigId * SigExp * SigBind option


  (* Specifications [Figures 5 and 7] *)

  and Spec' =
      VALSpec         of ValDesc
    | TYPESpec        of TypDesc
    | EQTYPESpec      of TypDesc
    | DATATYPESpec    of DatDesc
    | DATATYPE2Spec   of TyCon * longTyCon
    | EXCEPTIONSpec   of ExDesc
    | STRUCTURESpec   of StrDesc
    | INCLUDESpec     of SigExp
    | EMPTYSpec
    | SEQSpec         of Spec * Spec
    | SHARINGTYPESpec of Spec * longTyCon list
    | SHARINGSpec     of Spec * longStrId list

  and ValDesc' =
      ValDesc of VId * Ty * ValDesc option

  and TypDesc' =
      TypDesc of TyVar seq * TyCon * TypDesc option

  and DatDesc' =
      DatDesc of TyVar seq * TyCon * ConDesc * DatDesc option

  and ConDesc' =
      ConDesc of VId * Ty option * ConDesc option

  and ExDesc' =
      ExDesc of VId * Ty option * ExDesc option

  and StrDesc' =
      StrDesc of StrId * SigExp * StrDesc option


  (* Functors [Figures 5 and 8] *)

  and FunDec' =
      FunDec of FunBind

  and FunBind' =
      FunBind of FunId * StrId * SigExp * StrExp * FunBind option


  (* Top-level declarations [Figures 5 and 8] *)

  and TopDec' =
      STRDECTopDec of StrDec * TopDec option
    | SIGDECTopDec of SigDec * TopDec option
    | FUNDECTopDec of FunDec * TopDec option


  (* Annotated syntax *)

  withtype
      StrExp  = (StrExp', StrExp_attr) phrase
  and StrDec  = (StrDec', StrDec_attr) phrase
  and StrBind = (StrBind', StrBind_attr) phrase
  and SigExp  = (SigExp', SigExp_attr) phrase
  and SigDec  = (SigDec', SigDec_attr) phrase
  and SigBind = (SigBind', SigBind_attr) phrase
  and Spec    = (Spec', Spec_attr) phrase
  and ValDesc = (ValDesc', ValDesc_attr) phrase
  and TypDesc = (TypDesc', TypDesc_attr) phrase
  and DatDesc = (DatDesc', DatDesc_attr) phrase
  and ConDesc = (ConDesc', ConDesc_attr) phrase
  and ExDesc  = (ExDesc', ExDesc_attr) phrase
  and StrDesc = (StrDesc', StrDesc_attr) phrase
  and FunDec  = (FunDec', FunDec_attr) phrase
  and FunBind = (FunBind', FunBind_attr) phrase
  and TopDec  = (TopDec', TopDec_attr) phrase
end;
