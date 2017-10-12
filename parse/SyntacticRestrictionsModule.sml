(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic restrictions for modules
 *
 * Definition, Section 3.5
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: Local modules
 * + RFC: First-class modules
 *)

structure SyntacticRestrictionsModule : SYNTACTIC_RESTRICTIONS_MODULE =
struct
    (* Import *)

    open GrammarModule
    open BindingObjectsCore
    open BindingObjectsModule
    open Error


    (* Helpers for basis modification *)

    val empty  = BindingBasis.plus
    val plus   = BindingBasis.plus
    val plusG  = BindingBasis.plusG
    val plusE  = BindingBasis.plusE
    val plusSE = BindingBasis.plusSE

    infix plus plusG plusF plusE plusSE


    (* Inference rules [Section 5.7] *)


    (* Structure Expressions *)

    fun checkStrExp(C, STRUCTStrExp(I, dec)) =
	(* [RFC: Local modules] *)
	let
	    val E = SyntacticRestrictionsCore.checkDec(C, dec)
	in
	    Struct E
	end

      | checkStrExp(C, IDStrExp(I, longstrid)) =
	let
	    val M = case BindingContext.findLongStrId(C, longstrid)
		      of SOME M => M
		       | NONE   => Struct BindingEnv.empty (* actually an error *)
	in
	    M
	end

      | checkStrExp(C, COLONStrExp(I, strexp, sigexp)) =
	let
	    val M1 = checkStrExp(C, strexp)
	    val M2 = checkSigExp(BindingBasis.fromC C, sigexp)
	in
	    M2
	end

      | checkStrExp(C, SEALStrExp(I, strexp, sigexp)) =
	let
	    val M1 = checkStrExp(C, strexp)
	    val M2 = checkSigExp(BindingBasis.fromC C, sigexp)
	in
	    M2
	end

      | checkStrExp(C, UNPACKStrExp(I, atexp, sigexp)) =
	(* [RFC: First-class modules] *)
	let
	    val () = SyntacticRestrictionsCore.checkAtExp(C, atexp)
	    val M2 = checkSigExp(BindingBasis.fromC C, sigexp)
	in
	    M2
	end

      | checkStrExp(C, APPStrExp(I, strexp1, strexp2)) =
	(* [RFC: Higher-order functors] *)
	let
	    val M1 = checkStrExp(C, strexp1)
	    val M2 = checkStrExp(C, strexp2)
	in
	    M1
	end

      | checkStrExp(C, LETStrExp(I, dec, strexp)) =
	(* [RFC: Local modules] *)
	let
	    val E = SyntacticRestrictionsCore.checkDec(C, dec)
	    val M = checkStrExp(BindingContext.plusE(C, E), strexp)
	in
	    M
	end

      | checkStrExp(C, FCTStrExp(I, strid, sigexp, strexp)) =
	(* [RFC: Higher-order functors] *)
	let
	    val M1 = checkSigExp(BindingBasis.fromC C, sigexp)
	    val M2 = checkStrExp(BindingContext.plusSE(C,
					StrIdMap.singleton(strid, M1)), strexp)
	in
	    Functor(Fct M2)
	end

      | checkStrExp(C, PARStrExp(I, strexp)) =
	(* [RFC: Higher-order functors] *)
	let
	    val M = checkStrExp(C, strexp)
	in
	    M
	end


    (* Structure-level Declarations *)

    and checkStrDec(C, STRUCTUREStrDec(I, strbind)) =
	let
	    val SE = checkStrBind(C, strbind)
	in
	    BindingEnv.fromSE SE
	end

      | checkStrDec(C, SIGNATUREStrDec(I, sigbind)) =
	(* [RFC: Nested signatures] *)
	let
	    val G = checkSigBind(BindingBasis.fromC C, sigbind)
	in
	    BindingEnv.fromG G
	end

	(* Removed local [RFC: Local modules] *)

	(* Removed empty [RFC: Local modules] *)

	(* Removed sequential [RFC: Local modules] *)


    (* Structure Bindings *)

    and checkStrBind(C, StrBind(I, strid, strexp, strbind_opt)) =
	let
	    val M  = checkStrExp(C, strexp)
	    val SE = case strbind_opt
		       of NONE         => StrIdMap.empty
		        | SOME strbind => checkStrBind(C, strbind)
	in
	    if StrIdMap.inDomain(SE, strid) then
		(* [Section 3.5, 1st bullet] *)
		errorStrId(I, "duplicate structure identifier ", strid)
	    else
		StrIdMap.insert(SE, strid, M)
	end


    (* Signature Expressions *)

    and checkSigExp(B, SIGSigExp(I, spec)) =
	let
	    val E = checkSpec(B, spec)
	in
	    Struct E
	end

      (* [RFC: Nested signatures] *)
      | checkSigExp(B, IDSigExp(I, longsigid)) =
	let
	    val M = case BindingBasis.findLongSigId(B, longsigid)
		      of SOME M => M
		       | NONE   => Struct BindingEnv.empty (* actually an error *)
	in
	    M
	end

      | checkSigExp(B, WHERETYPESigExp(I, sigexp, tyvarseq, longtycon, ty)) =
	let
	    val M  = checkSigExp(B, sigexp)
	    val U1 = SyntacticRestrictionsCore.checkTyVarseq tyvarseq
	    val U2 = SyntacticRestrictionsCore.checkTy ty
	in
	    if not(TyVarSet.isSubset(U2, U1)) then
		(* [Section 3.5, 4th bullet] *)
		error(I, "free type variables in type realisation")
	    else
		M
	end

      (* [RFC: Higher-order functors] *)
      | checkSigExp(B, FCTSigExp(I, strid, sigexp1, sigexp2)) =
	let
	    val M1 = checkSigExp(B, sigexp1)
	    val M2 = checkSigExp(B plusSE StrIdMap.singleton(strid, M1), sigexp2)
	in
	    Functor(Fct M2)
	end

      (* [RFC: Higher-order functors] *)
      | checkSigExp(B, FCTSPECSigExp(I, spec, sigexp)) =
	let
	    val E = checkSpec(B, spec)
	    val M = checkSigExp(B plusE E, sigexp)
	in
	    M
	end

      (* [RFC: Higher-order functors] *)
      | checkSigExp(B, PARSigExp(I, sigexp)) =
	let
	    val M = checkSigExp(B, sigexp)
	in
	    M
	end


    (* Removed SigDec [RFC: Nested signatres] *)


    (* Signature Bindings *)

    and checkSigBind(B, SigBind(I, sigid, sigexp, sigbind_opt)) =
	let
	    val M = checkSigExp(B, sigexp)
	    val G = case sigbind_opt
		      of NONE         => SigIdMap.empty
		       | SOME sigbind => checkSigBind(B, sigbind)
	in
	    if SigIdMap.inDomain(G, sigid) then
		(* [Section 3.5, 1st bullet] *)
		errorSigId(I, "duplicate signature identifier ", sigid)
	    else
		SigIdMap.insert(G, sigid, Sig M)
	end


    (* Specifications *)

    and checkSpec(B, VALSpec(I, valdesc)) =
	let
	    val VE = checkValDesc valdesc
	in
	    BindingEnv.fromVE VE
	end

      | checkSpec(B, TYPESpec(I, typdesc)) =
	let
	    val TE = checkTypDesc typdesc
	in
	    BindingEnv.fromTE TE
	end

      | checkSpec(B, EQTYPESpec(I, typdesc)) =
	let
	    val TE = checkTypDesc typdesc
	in
	    BindingEnv.fromTE TE
	end

      | checkSpec(B, DATATYPESpec(I, datdesc)) =
	let
	    val (VE,TE) = checkDatDesc datdesc
	in
	    BindingEnv.fromVEandTE(VE,TE)
	end

      | checkSpec(C, VIEWTYPESpec(I, tyvarseq, tycon, ty, condesc)) =
	(* [RFC: Views] *)
	let
	    val  U1     = SyntacticRestrictionsCore.checkTyVarseq tyvarseq
	    val  U2     = SyntacticRestrictionsCore.checkTy ty
	    val (U3,VE) = checkConDesc condesc
	in
	    if not(TyVarSet.isSubset(TyVarSet.union(U2,U3), U1)) then
		error(I, "free type variables in viewtype description")
	    else
		BindingEnv.fromVEandTE (VE, TyConMap.singleton(tycon, VE))
	end

      | checkSpec(B, DATATYPE2Spec(I, tycon, longtycon)) =
	let
	    val VE = case BindingBasis.findLongTyCon(B, longtycon)
		       of SOME VE => VE
			| NONE    => VIdMap.empty (* actually an error *)
	    val TE = TyConMap.singleton(tycon, VE)
	in
	    BindingEnv.fromVEandTE(VE,TE)
	end

      | checkSpec(B, EXCEPTIONSpec(I, exdesc)) =
	let
	    val VE = checkExDesc exdesc
	in
	    BindingEnv.fromVE VE
	end

      | checkSpec(B, STRUCTURESpec(I, strdesc)) =
	let
	    val SE = checkStrDesc(B, strdesc)
	in
	    BindingEnv.fromSE SE
	end

      (* [RFC: Nested signatures] *)
      | checkSpec(B, SIGNATURESpec(I, sigdesc)) =
	let
	    val G = checkSigDesc(B, sigdesc)
	in
	    BindingEnv.fromG G
	end

      | checkSpec(B, INCLUDESpec(I, sigexp)) =
	let
	    val E = case checkSigExp(B, sigexp)
		      of Struct E  => E
		       | Functor _ => BindingEnv.empty (* actually an error *)
	in
	    E
	end

      | checkSpec(B, EMPTYSpec(I)) =
	    BindingEnv.empty

      | checkSpec(B, SEQSpec(I, spec1, spec2)) =
	let
	    val E1 = checkSpec(B, spec1)
	    val E2 = checkSpec(B plusE E1, spec2)
	in
	    BindingEnv.plus(E1, E2)
	end

      | checkSpec(B, SHARINGTYPESpec(I, spec, longtycons)) =
	let
	    val E = checkSpec(B, spec)
	in
	    E
	end

      | checkSpec(B, SHARINGSpec(I, spec, longstrids)) =
	(* [Appendix A] *)
	let
	    val E = checkSpec(B, spec)
	in
	    E
	end


    (* Value Descriptions *)

    and checkValDesc(ValDesc(I, vid, ty, valdesc_opt)) =
	let
	    val U  = SyntacticRestrictionsCore.checkTy ty
	    val VE = case valdesc_opt
		       of NONE         => VIdMap.empty
			| SOME valdesc => checkValDesc valdesc
	in
	    if VIdMap.inDomain(VE, vid) then
		(* [Section 3.5, 2nd bullet] *)
		errorVId(I, "duplicate variable ", vid)
	    else if not(SyntacticRestrictionsCore.validBindVId vid) then
		(* [Section 3.5, 5th bullet] *)
		errorVId(I, "illegal specification of identifier ", vid)
	    else
		VIdMap.insert(VE, vid, IdStatus.v)
	end


    (* Type Descriptions *)

    and checkTypDesc(TypDesc(I, tyvarseq, tycon, typdesc_opt)) =
	let
	    val U  = SyntacticRestrictionsCore.checkTyVarseq tyvarseq
	    val TE = case typdesc_opt
		       of NONE         => TyConMap.empty
			| SOME typdesc => checkTypDesc typdesc
	in
	    if TyConMap.inDomain(TE, tycon) then
		(* [Section 3.5, 2nd bullet] *)
		errorTyCon(I, "duplicate type constructor ", tycon)
	    else
		TyConMap.insert(TE, tycon, VIdMap.empty)
	end


    (* Datatype Descriptions *)

    and checkDatDesc(DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
	let
	    val  U1      = SyntacticRestrictionsCore.checkTyVarseq tyvarseq
	    val (U2,VE)  = checkConDesc condesc
	    val(VE',TE') = case datdesc_opt
			     of NONE         => ( VIdMap.empty, TyConMap.empty )
			      | SOME datdesc => checkDatDesc datdesc
	in
	    if TyConMap.inDomain(TE', tycon) then
		(* [Section 3.5, 2nd bullet] *)
		errorTyCon(I, "duplicate type constructor ", tycon)
	    else if not(TyVarSet.isSubset(U2, U1)) then
		(* [Section 3.5,4th bullet]*)
		error(I, "free type variables in datatype description")
	    else
	    ( VIdMap.unionWithi (fn(vid,_,_) =>
		(* [Section 3.5, 2nd bullet] *)
		errorVId(I, "duplicate data cnstructor ", vid)) (VE,VE')
	    , TyConMap.insert(TE', tycon, VE)
	    )
	end


    (* Constructor Descriptions *)

    and checkConDesc(ConDesc(I, vid, ty_opt, condesc_opt)) =
	let
	    val  U      = case ty_opt
			    of NONE    => TyVarSet.empty
			     | SOME ty => SyntacticRestrictionsCore.checkTy ty
	    val (U',VE) = case condesc_opt
			    of NONE         => ( TyVarSet.empty, VIdMap.empty )
			     | SOME condesc => checkConDesc condesc
	in
	    if VIdMap.inDomain(VE, vid) then
		(* [Section 3.5, 2nd bullet] *)
		errorVId(I, "duplicate data constructor ", vid)
	    else if not(SyntacticRestrictionsCore.validConBindVId vid) then
		(* [Section 3.5, 5th bullet] *)
		errorVId(I, "illegal specifiation of identifier ", vid)
	    else
		( TyVarSet.union(U, U'), VIdMap.insert(VE, vid, IdStatus.c) )
	end


    (* Exception Description *)

    and checkExDesc(ExDesc(I, vid, ty_opt, exdesc_opt)) =
	let
	    val U  = case ty_opt
		       of NONE    => TyVarSet.empty
			| SOME ty => SyntacticRestrictionsCore.checkTy ty
	    val VE = case exdesc_opt
		       of NONE        => VIdMap.empty
			| SOME exdesc => checkExDesc exdesc
	in
	    if VIdMap.inDomain(VE, vid) then
		(* [Section 3.5, 2nd bullet] *)
		errorVId(I, "duplicate exception constructor ", vid)
	    else if not(SyntacticRestrictionsCore.validConBindVId vid) then
		(* [Section 3.5, 5th bullet] *)
		errorVId(I, "illegal specification of identifier ", vid)
	    else
		VIdMap.insert(VE, vid, IdStatus.e)
	end


    (* Structure Descriptions *)

    and checkStrDesc(B, StrDesc(I, strid, sigexp, strdesc_opt)) =
	let
	    val M  = checkSigExp(B, sigexp)
	    val SE = case strdesc_opt
		       of NONE         => StrIdMap.empty
		        | SOME strdesc => checkStrDesc(B, strdesc)
	in
	    if StrIdMap.inDomain(SE, strid) then
		(* [Section 3.5, 2nd bullet] *)
		errorStrId(I, "duplicate structure identifier ", strid)
	    else
		StrIdMap.insert(SE, strid, M)
	end


    (* Signature Descriptions [RFC: Nested signatures] *)

    and checkSigDesc(B, SigDesc(I, sigid, sigexp, sigdesc_opt)) =
	let
	    val M = checkSigExp(B, sigexp)
	    val G = case sigdesc_opt
		      of NONE         => SigIdMap.empty
		       | SOME sigdesc => checkSigDesc(B, sigdesc)
	in
	    if SigIdMap.inDomain(G, sigid) then
		(* [Section 3.5, 1st bullet] *)
		errorSigId(I, "duplicate signature identifier ", sigid)
	    else
		SigIdMap.insert(G, sigid, Sig M)
	end


    (* Removed FunDec [RFC: Higher-order functors] *)

    (* Removed FunBind [RFC: Higher-order functors] *)


    (* Top-level Declarations *)

    and checkTopDec(B, TopDec(I, dec)) =
	(* [RFC: Local modules] *)
	let
	    val E = SyntacticRestrictionsCore.checkDec(BindingBasis.Cof B, dec)
	in
	    BindingBasis.fromE E
	end

      (* Removed SigDec [RFC: Nested signatures] *)

      (* Removed FunDec [RFC: Higher-order functors] *)


    (* Tie recursive imports *)

    val _ = SyntacticRestrictionsCore.SyntacticRestrictionsModule.checkStrDec :=
	    (fn (C, StrDec strdec) => checkStrDec(C, strdec)
	      | _ => raise Fail "SyntacticRestrictionsModule.checkStrDec: invalid declaration")
end;
