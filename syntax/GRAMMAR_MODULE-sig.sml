(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML abstract module grammar
 *
 * Definition, Section 3.4
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Notes:
 *   This is the syntax used in the inference rules for modules [Definition,
 *   Sections 5.7 and 7.3]. Optional semicolons are omitted.
 *   The structure sharing derived form [Definition, Appendix A] has been added,
 *   because it cannot be derived purely syntactically.
 *   Likewise, the functor signature derived form with a spec argument. 
 *)

signature GRAMMAR_MODULE =
sig
    (* Import *)

    structure Core : GRAMMAR_CORE

    type Info

    type VId		= Core.VId
    type TyCon		= Core.TyCon
    type TyVar		= Core.TyVar
    type StrId		= Core.StrId
    type longVId	= Core.longVId
    type longTyCon	= Core.longTyCon
    type longStrId	= Core.longStrId
    type AtExp		= Core.AtExp
    type Dec		= Core.Dec
    type Ty		= Core.Ty
    type TyVarseq	= Core.TyVarseq

    type SigId		= SigId.Id
    type longSigId	= LongSigId.longId


    (* Structures [Figures 5 and 6; RFC: Higher-order functors;
     *                              RFC: Nested signatures;
     *                              RFC: Local modules] *)

    datatype StrExp =
	  STRUCTStrExp    of Info * Dec
	| IDStrExp        of Info * longStrId
	| COLONStrExp     of Info * StrExp * SigExp
	| SEALStrExp      of Info * StrExp * SigExp
	| UNPACKStrExp    of Info * AtExp * SigExp
	| APPStrExp       of Info * StrExp * StrExp
	| LETStrExp       of Info * Dec * StrExp
	| FCTStrExp       of Info * StrId * SigExp * StrExp
	| PARStrExp       of Info * StrExp

    and StrDec =
          STRUCTUREStrDec of Info * StrBind
        | SIGNATUREStrDec of Info * SigBind

    and StrBind =
          StrBind         of Info * StrId * StrExp * StrBind option

    (* Signatures [Figures 5 and 6; RFC: Higher-order functors;
     *                              RFC: Nested signatures] *)

    and SigExp =
          SIGSigExp       of Info * Spec
	| IDSigExp        of Info * longSigId
        | WHERETYPESigExp of Info * SigExp * TyVarseq * longTyCon * Ty
	| FCTSigExp       of Info * StrId * SigExp * SigExp
	| FCTSPECSigExp   of Info * Spec * SigExp
	| PARSigExp       of Info * SigExp

    (* Removed SigDec [RFC: Nested signatures] *)

    and SigBind =
          SigBind         of Info * SigId * SigExp * SigBind option

    (* Specifications [Figures 5 and 7; RFC: Nested signatures] *)

    and Spec =
	  VALSpec         of Info * ValDesc
	| TYPESpec        of Info * TypDesc
	| EQTYPESpec      of Info * TypDesc
	| DATATYPESpec    of Info * DatDesc
	| VIEWTYPESpec    of Info * TyVarseq * TyCon * Ty * ConDesc
	| DATATYPE2Spec   of Info * TyCon * longTyCon
	| EXCEPTIONSpec   of Info * ExDesc
	| STRUCTURESpec   of Info * StrDesc
        | SIGNATURESpec   of Info * SigDesc
	| INCLUDESpec     of Info * SigExp
	| EMPTYSpec       of Info
	| SEQSpec         of Info * Spec * Spec
	| SHARINGTYPESpec of Info * Spec * longTyCon list
	| SHARINGSpec     of Info * Spec * longStrId list

    and ValDesc =
	  ValDesc         of Info * VId * Ty * ValDesc option

    and TypDesc =
	  TypDesc         of Info * TyVarseq * TyCon * TypDesc option

    and DatDesc =
	  DatDesc         of Info * TyVarseq * TyCon * ConDesc * DatDesc option

    and ConDesc =
	  ConDesc         of Info * VId * Ty option * ConDesc option

    and ExDesc =
	  ExDesc          of Info * VId * Ty option * ExDesc option

    and StrDesc =
          StrDesc         of Info * StrId * SigExp * StrDesc option

    (* [RFC: Nested signatures] *)
    and SigDesc =
          SigDesc         of Info * SigId * SigExp * SigDesc option

    (* Removed FunDec [RFC: Higher-order functors] *)

    (* Removed FunBind [RFC: Higher-order functors] *)

    (* Top-level declarations [Figures 5 and 8; RFC: Higher-order functors;
     *                                          RFC: Nested signatures;
     *                                          RFC: Local modules] *)

    and TopDec =
          TopDec          of Info * Dec


    (* Inter-module recursive types *)

    exception StrDec      of StrDec


    (* Operations *)

    val infoStrExp :	StrExp  -> Info
    val infoStrDec :	StrDec  -> Info
    val infoStrBind :	StrBind -> Info
    val infoSigExp :	SigExp  -> Info
    val infoSigBind :	SigBind -> Info
    val infoSpec :	Spec    -> Info
    val infoValDesc :	ValDesc -> Info
    val infoTypDesc :	TypDesc -> Info
    val infoDatDesc :	DatDesc -> Info
    val infoConDesc :	ConDesc -> Info
    val infoExDesc :	ExDesc  -> Info
    val infoStrDesc :	StrDesc -> Info
    val infoSigDesc :	SigDesc -> Info
    val infoTopDec :	TopDec  -> Info
end;
