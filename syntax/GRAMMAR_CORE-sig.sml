(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML abstract core grammar
 *
 * Definition, Section 2.8
 * + RFC: Record extension
 * + RFC: Conjunctive patterns
 * + RFC: Disjunctive patterns
 * + RFC: Nested matches
 * + RFC: Views
 * + RFC: Simplified recursive value bindings
 * + RFC: Abstype as derived
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Note:
 *   This is the syntax used in the inference rules for the core [Definition,
 *   Sections 4.10 and 6.7]. It omits almost anything having to do with infixed
 *   identifiers:
 *     - fixity directives
 *     - infixed application
 *     - infixed value construction
 *   However, op prefixes are kept, since they are required for rebuilding the
 *   syntax tree during fixity resolution.
 *   Optional semicolons are also omitted.
 *)

signature GRAMMAR_CORE =
sig
    (* Import *)

    type Info

    type SCon		= SCon.SCon
    type Lab		= Lab.Lab
    type VId		= VId.Id
    type TyCon		= TyCon.Id
    type TyVar		= TyVar.TyVar
    type StrId		= StrId.Id
    type longVId	= LongVId.longId
    type longTyCon	= LongTyCon.longId
    type longStrId	= LongStrId.longId
    type longSigId	= LongSigId.longId

    (* Recursive import *)

    type StrDec'	= exn

    (* Optional keywords `op' and `rec' *)

    datatype Op  = SANSOp | WITHOp
    datatype Rec = SANSRec | WITHRec


    (* Expressions [Figures 2 and 4; RFC: Record extension] *)

    datatype AtExp =
	  SCONAtExp      of Info * SCon
	| IDAtExp        of Info * Op * longVId
	| RECORDAtExp    of Info * ExpRow option
	| LETAtExp       of Info * Dec * Exp
	| PARAtExp       of Info * Exp

    and ExpRow =
	  DOTSExpRow     of Info * Exp
	| FIELDExpRow    of Info * Lab * Exp * ExpRow option

    and Exp =
	  ATExp          of Info * AtExp
	| APPExp         of Info * Exp * AtExp
	| COLONExp       of Info * Exp * Ty
	| PACKExp        of Info * longStrId * longSigId
	| HANDLEExp      of Info * Exp * Match
	| RAISEExp       of Info * Exp
	| FNExp          of Info * Match

    (* Matches [Figures 2 and 4] *)

    and Match =
	  Match          of Info * Mrule * Match option

    and Mrule =
	  Mrule          of Info * Pat * Exp

    (* Declarations [Figures 2 and 4;
     *               RFC: Simplified recursive value bindings;
     *               RFC: Abstype as derived;
     *               RFC: Local modules] *)

    and Dec =
	  VALDec         of Info * Rec * TyVarseq * ValBind
	| TYPEDec        of Info * TypBind
	| DATATYPEDec    of Info * DatBind
	| VIEWTYPEDec    of Info * TyVarseq * TyCon * Ty * ConBind * Dec
	| DATATYPE2Dec   of Info * TyCon * longTyCon
	| EXCEPTIONDec   of Info * ExBind
	| STRDECDec      of Info * StrDec'
	| LOCALDec       of Info * Dec * Dec
	| OPENDec        of Info * longStrId list
	| EMPTYDec       of Info
	| SEQDec         of Info * Dec * Dec

    (* Bindings [Figures 2 and 4; RFC: Simplified recursive value bindings] *)

    and ValBind =
	  ValBind        of Info * Pat * Exp * ValBind option

    and TypBind =
	  TypBind        of Info * TyVarseq * TyCon * Ty * TypBind option

    and DatBind =
	  DatBind        of Info * TyVarseq * TyCon * ConBind * DatBind option

    and ConBind =
	  ConBind        of Info * Op * VId * Ty option * ConBind option

    and ExBind =
	  NEWExBind      of Info * Op * VId * Ty option * ExBind option
	| EQUALExBind    of Info * Op * VId * Op * longVId * ExBind option

    (* Patterns [Figures 2 and 3; RFC: Record extension;
     *                            RFC: Conjunctive patterns;
     *                            RFC: Disjunctive patterns;
     *                            RFC: Nested matches] *)

    and AtPat =
	  WILDCARDAtPat  of Info
	| SCONAtPat      of Info * SCon
	| IDAtPat        of Info * Op * longVId
	| RECORDAtPat    of Info * PatRow option
	| PARAtPat       of Info * Pat

    and PatRow =
	  DOTSPatRow     of Info * Pat
	| FIELDPatRow    of Info * Lab * Pat * PatRow option

    and Pat =
	  ATPat          of Info * AtPat
	| CONPat         of Info * Op * longVId * AtPat
	| COLONPat       of Info * Pat * Ty
	| ASPat          of Info * Pat * Pat
	| BARPat         of Info * Pat * Pat
	| WITHPat        of Info * Pat * Pat * Exp

    (* Type expressions [Figures 2 and 3; RFC: Record extension] *)

    and Ty =
	  VARTy          of Info * TyVar
	| RECORDTy       of Info * TyRow option
	| CONTy          of Info * Tyseq * longTyCon
	| ARROWTy        of Info * Ty * Ty
	| PACKTy         of Info * longSigId
	| PARTy          of Info * Ty

    and TyRow =
	  DOTSTyRow      of Info * Ty
	| FIELDTyRow     of Info * Lab * Ty * TyRow option

    (* Sequences [Section 2.8] *)

    and Tyseq =
	  Tyseq          of Info * Ty list

    and TyVarseq =
	  TyVarseq       of Info * TyVar list


    (* Operations *)

    val infoAtExp :	AtExp	-> Info
    val infoExpRow :	ExpRow	-> Info
    val infoExp :	Exp	-> Info
    val infoMatch :	Match	-> Info
    val infoMrule :	Mrule	-> Info
    val infoDec :	Dec	-> Info
    val infoValBind :	ValBind	-> Info
    val infoTypBind :	TypBind	-> Info
    val infoDatBind :	DatBind	-> Info
    val infoConBind :	ConBind	-> Info
    val infoExBind :	ExBind	-> Info
    val infoAtPat :	AtPat	-> Info
    val infoPatRow :	PatRow	-> Info
    val infoPat :	Pat	-> Info
    val infoTy :	Ty	-> Info
    val infoTyRow :	TyRow	-> Info
    val infoTyseq :	Tyseq	-> Info
    val infoTyVarseq :	TyVarseq -> Info
end;
