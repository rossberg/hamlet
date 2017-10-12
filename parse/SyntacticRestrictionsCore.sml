(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic restrictions for the core
 *
 * Definition, Section 2.9
 * + RFC: Syntax fixes
 * + RFC: Semantic fixes
 * + RFC: Record extension
 * + RFC: Conjunctive patterns
 * + RFC: Disjunctive patterns
 * + RFC: Nested matches
 * + RFC: Transformation patterns
 * + RFC: Views
 * + RFC: Simplified recursive value bindings
 * + RFC: Abstype as derived
 * + RFC: Higher-order functors
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Notes:
 *   - The "syntactic restrictions" defined in the Definition are not purely
 *     syntactic. E.g. the restriction that valbinds may not bind the same vid
 *     twice [2nd bullet] cannot be checked without proper binding analysis,
 *     to compute identifier status.
 *   - Also, checking of type variable shadowing [last bullet] is a global
 *     check dependent on context. Moreover, it requires the transformation from
 *     Section 4.6 to be done first.
 *)

structure SyntacticRestrictionsCore : SYNTACTIC_RESTRICTIONS_CORE =
struct
    (* Import *)

    open GrammarCore
    open BindingObjectsCore
    open Error


    (* Recursive import *)

    structure SyntacticRestrictionsModule =
    struct
	val checkStrDec : (Context * StrDec' -> Env) ref =
	    ref (fn _ => raise Fail "SyntacticRestrictionsCore.SyntacticRestrictionsModule.checkStrDec")
    end


    (* Helpers for context modification *)

    open BindingContext
    val plus = BindingEnv.plus

    infix plus plusU plusE plusVE plusTE plusVEandTE plusSE
    

    (* Checking restriction for vids in binding [Section 2.9, 5th bullet;
     *                                           RFC: Transformation patterns] *)

    fun validBindVId vid =
	    vid <> VId.fromString "true" andalso
	    vid <> VId.fromString "false" andalso
	    vid <> VId.fromString "SOME" andalso
	    vid <> VId.fromString "NONE" andalso
	    vid <> VId.fromString "nil" andalso
	    vid <> VId.fromString "::"  andalso
	    vid <> VId.fromString "ref"

    fun validConBindVId vid =
	    validBindVId vid andalso
	    vid <> VId.fromString "it"


    (* Type variable sequences *)

    fun checkTyVarseq(TyVarseq(I, tyvars)) =
	(* [Section 2.9, 3rd bullet; Section 3.5, 3rd bullet] *)
	let
	    fun check(U, []) = U
	      | check(U, tyvar::tyvars) =
		    if TyVarSet.member(U, tyvar) then
			errorTyVar(I, "duplicate type variable ", tyvar)
		    else
			check(TyVarSet.add(U, tyvar), tyvars)
	in
	    check(TyVarSet.empty, tyvars)
	end


    (* Atomic Expressions *)

    fun checkAtExp(C, SCONAtExp(I, scon)) =
	    ()

      | checkAtExp(C, IDAtExp(I, _, longvid)) =
	    ()

      | checkAtExp(C, RECORDAtExp(I, exprow_opt)) =
	let
	    (* [RFC: Record extension] *)
	    val () = case exprow_opt
		       of NONE        => ()
		        | SOME exprow => checkExpRow(C, exprow)
	in
	    ()
	end

      | checkAtExp(C, LETAtExp(I, dec, exp)) =
	let
	    val E = checkDec(C, dec)
	in
	    checkExp(C plusE E, exp)
	end

      | checkAtExp(C, PARAtExp(I, exp)) =
	let
	    val () = checkExp(C, exp)
	in
	    ()
	end


    (* Expression Rows *)

    and checkExpRow(C, FIELDExpRow(I, lab, exp, exprow_opt)) =
	let
	    val () = checkExp(C, exp)
	    val () = case exprow_opt
		       of NONE        => ()
		        | SOME exprow => checkExpRow(C, exprow)
	in
	    (* [RFC: Record extension] *)
	    ()
	end

      (* [RFC: Record extension] *)
      | checkExpRow(C, DOTSExpRow(I, exp)) =
	let
	    val () = checkExp(C, exp)
	in
	    ()
	end


    (* Expressions *)

    and checkExp(C, ATExp(I, atexp)) =
	let
	    val () = checkAtExp(C, atexp)
	in
	    ()
	end

      | checkExp(C, APPExp(I, exp, atexp)) =
	let
	    val () = checkExp(C, exp)
	    val () = checkAtExp(C, atexp)
	in
	    ()
	end

      | checkExp(C, COLONExp(I, exp, ty)) =
	let
	    val () = checkExp(C, exp)
	    val U  = checkTy ty
	in
	    ()
	end

      | checkExp(C, PACKExp(I, longstrid, longsigid)) =
	(* [RFC: First-class modules] *)
	    ()

      | checkExp(C, HANDLEExp(I, exp, match)) =
	let
	    val () = checkExp(C, exp)
	    val () = checkMatch(C, match)
	in
	    ()
	end

      | checkExp(C, RAISEExp(I, exp)) =
	let
	    val () = checkExp(C, exp)
	in
	    ()
	end

      | checkExp(C, FNExp(I, match)) =
	let
	    val () = checkMatch(C, match)
	in
	    ()
	end


    (* Matches *)

    and checkMatch(C, Match(I, mrule, match_opt)) =
	let
	    val () = checkMrule(C, mrule)
	    val () = case match_opt
		       of NONE       => ()
			| SOME match => checkMatch(C, match)
	in
	    ()
	end


    (* Match rules *)

    and checkMrule(C, Mrule(I, pat, exp)) =
	let
	    val VE = checkPat(C, pat)
	    val () = checkExp(C plusVE VE, exp)
	in
	    ()
	end


    (* Declarations *)

    and checkDec(C, VALDec(I, rec_opt, tyvarseq, valbind)) =
	(* [RFC: Simplified recursive value bindings] *)
	let
	    val U' = checkTyVarseq tyvarseq
	    (* Collect implicitly bound tyvars [Section 4.6] *)
	    val U  = TyVarSet.union(U',
			TyVarSet.difference(ScopeTyVars.unguardedTyVars valbind,
					    Uof C))
	    val VE1 = if rec_opt = WITHRec then lhsRecValBind(C, valbind)
		      else VIdMap.empty
	    val VE  = checkValBind(C plusU U plusVE VE1, valbind)
	in
	    if not(TyVarSet.isEmpty(TyVarSet.intersection(Uof C, U))) then
		(* [Section 2.9, last bullet] *)
		error(I, "some type variables shadow previous ones")
	    else
		BindingEnv.fromVE VE
	end

      | checkDec(C, TYPEDec(I, typbind)) =
	let
	    val TE = checkTypBind typbind
	in
	    BindingEnv.fromTE TE
	end

      | checkDec(C, DATATYPEDec(I, datbind)) =
	let
	    val (VE,TE) = checkDatBind datbind
	in
	    BindingEnv.fromVEandTE(VE,TE)
	end

      | checkDec(C, VIEWTYPEDec(I, tyvarseq, tycon, ty, conbind, dec)) =
	(* [RFC: Views] *)
	let
	    val  U1     = checkTyVarseq tyvarseq
	    val  U2     = checkTy ty
	    val (U3,VE) = checkConBind conbind
	    val  TE     = TyConMap.singleton(tycon, VE)
	    val  E      = checkDec(C plusVEandTE (VE,TE), dec)
	in
	    if not(TyVarSet.isSubset(TyVarSet.union(U2,U3), U1)) then
		error(I, "free type variables in viewtype binding")
	    else
		BindingEnv.fromVEandTE (VE,TE)
	end

      | checkDec(C, DATATYPE2Dec(I, tycon, longtycon)) =
	let
	    val VE = case findLongTyCon(C, longtycon)
		       of SOME VE => VE
			| NONE    => VIdMap.empty (* actually an error *)
	    val TE = TyConMap.singleton(tycon, VE)
	in
	    BindingEnv.fromVEandTE(VE,TE)
	end

      (* Removed ABSTYPEDec [RFC: Abstype as derived] *)

      | checkDec(C, EXCEPTIONDec(I, exbind)) =
	let
	    val VE = checkExBind exbind
	in
	    BindingEnv.fromVE VE
	end

      | checkDec(C, STRDECDec(I, strdec)) =
	(* [RFC: Local modules] *)
	let
	    val E = !SyntacticRestrictionsModule.checkStrDec(C, strdec)
	in
	    E
	end

      | checkDec(C, LOCALDec(I, dec1, dec2)) =
	let
	    val E1 = checkDec(C, dec1)
	    val E2 = checkDec(C plusE E1, dec2)
	in
	    E2
	end

      | checkDec(C, OPENDec(I, longstrids)) =
	let
	    val Es =
		List.map
		    (fn longstrid =>
			case findLongStrId(C, longstrid)
			  (* [RFC: Higher-order functors] *)
			  of SOME(Struct E) => E
			   | _ => BindingEnv.empty) (* actually an error *)
		    longstrids
	in
	    List.foldl (op plus) BindingEnv.empty Es
	end

      | checkDec(C, EMPTYDec(I)) =
	    BindingEnv.empty

      | checkDec(C, SEQDec(I, dec1, dec2)) =
	let
	    val E1 = checkDec(C, dec1)
	    val E2 = checkDec(C plusE E1, dec2)
	in
	    E1 plus E2
	end


    (* Value Bindings *)

    and checkValBind(C, ValBind(I, pat, exp, valbind_opt)) =
	let
	    val VE  = checkPat(C, pat)
	    val ()  = checkExp(C, exp)
	    val VE' = case valbind_opt
			of NONE         => VIdMap.empty
			 | SOME valbind => checkValBind(C, valbind)
	in
	    VIdMap.appi (fn(vid,_) =>
		if validBindVId vid then () else
		(* [Section 2.9, 5th bullet] *)
		errorVId(I, "illegal rebinding of identifier ", vid)
	    ) VE;
	    VIdMap.unionWithi
		(fn(vid,_,_) =>
		    (* [Section 2.9, 2nd bullet] *)
		    errorVId(I, "duplicate variable ", vid))
		(VE,VE')
	end

	(* Removed RECValBind [RFC: Simplified recursive value bindings] *)


    (* Type Bindings *)

    and checkTypBind(TypBind(I, tyvarseq, tycon, ty, typbind_opt)) =
	let
	    val U1 = checkTyVarseq tyvarseq
	    val U2 = checkTy ty
	    val TE = case typbind_opt
		       of NONE         => TyConMap.empty
			| SOME typbind => checkTypBind typbind
	in
	    if not(TyVarSet.isSubset(U2, U1)) then
		(* [RFC: Syntax fixes; RFC: Semantic fixes] *)
		error(I, "free type variables in type binding")
	    else if TyConMap.inDomain(TE, tycon) then
		(* Syntactic restriction [Section 2.9, 2nd bullet] *)
		errorTyCon(I, "duplicate type constructor ", tycon)
	    else
		TyConMap.insert(TE, tycon, VIdMap.empty)
	end


    (* Datatype Bindings *)

    and checkDatBind(DatBind(I, tyvarseq, tycon, conbind, datbind_opt)) =
	let
	    val  U1     = checkTyVarseq tyvarseq
	    val (U2,VE) = checkConBind conbind
	    val(VE',TE') = case datbind_opt
			     of NONE         => ( VIdMap.empty, TyConMap.empty )
			      | SOME datbind => checkDatBind datbind
	in
	    if not(TyVarSet.isSubset(U2, U1)) then
		(* [RFC: Syntax fixes; RFC: Semantic fixes] *)
		error(I, "free type variables in datatype binding")
	    else if TyConMap.inDomain(TE', tycon) then
		(* [Section 2.9, 2nd bullet] *)
		errorTyCon(I, "duplicate type constructor ", tycon)
	    else
	    ( VIdMap.unionWithi (fn(vid,_,_) =>
		(* [Section 2.9, 2nd bullet] *)
		errorVId(I, "duplicate data constructor ", vid)) (VE,VE')
	    , TyConMap.insert(TE', tycon, VE)
	    )
	end


    (* Constructor Bindings *)

    and checkConBind(ConBind(I, _, vid, ty_opt, conbind_opt)) =
	let
	    val  U      = case ty_opt
			    of NONE    => TyVarSet.empty
			     | SOME ty => checkTy ty
	    val (U',VE) = case conbind_opt
			    of NONE         => ( TyVarSet.empty, VIdMap.empty )
			     | SOME conbind => checkConBind conbind
	in
	    if VIdMap.inDomain(VE, vid) then
		(* [Section 2.9, 2nd bullet] *)
		errorVId(I, "duplicate data constructor ", vid)
	    else if not(validConBindVId vid) then
		(* [Section 2.9, 5th bullet] *)
		errorVId(I, "illegal rebinding of identifier ", vid)
	    else
		( TyVarSet.union(U, U'), VIdMap.insert(VE, vid, IdStatus.c) )
	end


    (* Exception Bindings *)

    and checkExBind(NEWExBind(I, _, vid, ty_opt, exbind_opt)) =
	let
	    val U  = case ty_opt
			 of NONE    => TyVarSet.empty
			  | SOME ty => checkTy ty
	    val VE = case exbind_opt
		       of NONE        => VIdMap.empty
		        | SOME exbind => checkExBind exbind
	in
	    if VIdMap.inDomain(VE, vid) then
		(* [Section 2.9, 2nd bullet] *)
		errorVId(I, "duplicate exception constructor ", vid)
	    else if not(validConBindVId vid) then
		(* [Section 2.9, 5th bullet] *)
		errorVId(I, "illegal rebinding of identifier ", vid)
	    else
		VIdMap.insert(VE, vid, IdStatus.e)
	end

      | checkExBind(EQUALExBind(I, _, vid, _, longvid, exbind_opt)) =
	let
	    val VE = case exbind_opt
		       of NONE        => VIdMap.empty
		        | SOME exbind => checkExBind exbind
	in
	    if VIdMap.inDomain(VE, vid) then
		(* [Section 2.9, 2nd bullet] *)
		errorVId(I, "duplicate exception constructor ", vid)
	    else
		VIdMap.insert(VE, vid, IdStatus.e)
	end


    (* Atomic Patterns *)

    and checkAtPat(C, WILDCARDAtPat(I)) =
	    VIdMap.empty

      | checkAtPat(C, SCONAtPat(I, scon)) =
	(case scon
	   of SCon.REAL _ =>
	      (* [Section 2.9, 6th bullet] *)
	      error(I, "real constant in pattern")
	    | _ =>
	      VIdMap.empty
	)

      | checkAtPat(C, IDAtPat(I, _, longvid)) =
	let
	    val (strids,vid) = LongVId.explode longvid
	in
	    if List.null strids andalso
	       ( case findLongVId(C, longvid)
		   of NONE    => true
		    | SOME is => is = IdStatus.v )
	    then
		VIdMap.singleton(vid, IdStatus.v)
	    else
		VIdMap.empty
	end

      | checkAtPat(C, RECORDAtPat(I, patrow_opt)) =
	let
	    (* [RFC: Record extension] *)
	    val VE = case patrow_opt
		       of NONE        => VIdMap.empty
			| SOME patrow => checkPatRow(C, patrow)
	in
	    VE
	end

      | checkAtPat(C, PARAtPat(I, pat)) =
	let
	    val VE = checkPat(C, pat)
	in
	    VE
	end


    (* Pattern Rows *)

    (* [RFC: Record extension] *)
    and checkPatRow(C, DOTSPatRow(I, pat)) =
	let
	    val VE = checkPat(C, pat)
	in
	    VE
	end

      | checkPatRow(C, FIELDPatRow(I, lab, pat, patrow_opt)) =
	let
	    val VE  = checkPat(C, pat)
	    val VE' = case patrow_opt
			of NONE        => VIdMap.empty
			 | SOME patrow => checkPatRow(C plusVE VE, patrow)
	in
	    (* [RFC: Record extension] *)
	    VIdMap.unionWithi #2 (VE,VE')
	end


    (* Patterns *)

    and checkPat(C, ATPat(I, atpat)) =
	let
	    val VE = checkAtPat(C, atpat)
	in
	    VE
	end

      | checkPat(C, CONPat(I, _, longvid, atpat)) =
	let
	    val VE = checkAtPat(C, atpat)
	in
	    VE
	end

      | checkPat(C, COLONPat(I, pat, ty)) =
	let
	    val VE = checkPat(C, pat)
	    val U  = checkTy ty
	in
	    VE
	end

      (* [RFC: Conjunctive patterns; RFC: Nested matches] *)
      | checkPat(C, ASPat(I, pat1, pat2)) =
	let
	    val VE1 = checkPat(C, pat1)
	    val VE2 = checkPat(C plusVE VE1, pat2)
	in
	    VIdMap.unionWithi (fn(vid,_,_) =>
		(* [Section 2.9, 2nd bullet] *)
		errorVId(I, "duplicate variable ", vid)) (VE1,VE2)
	end

      (* [RFC: Disjunctive patterns] *)
      | checkPat(C, BARPat(I, pat1, pat2)) =
	let
	    val VE1 = checkPat(C, pat1)
	    val VE2 = checkPat(C, pat2)
	in
	    VIdMap.unionWith #2 (VE1,VE2)
	end

      (* [RFC: Nested matches] *)
      | checkPat(C, WITHPat(I, pat1, pat2, exp)) =
	let
	    val VE1 = checkPat(C, pat1)
	    val VE2 = checkPat(C plusVE VE1, pat2)
	    val ()  = checkExp(C plusVE VE1, exp)
	in
	    VIdMap.unionWithi (fn(vid,_,_) =>
		(* [Section 2.9, 2nd bullet] *)
		errorVId(I, "duplicate variable ", vid)) (VE1,VE2)
	end


    (* Type Expressions *)

    and checkTy(VARTy(I, tyvar)) =
	    TyVarSet.singleton tyvar

      | checkTy(RECORDTy(I, tyrow_opt)) =
	let
	    (* [RFC: Record extension] *)
	    val U = case tyrow_opt
		      of NONE       => TyVarSet.empty
		       | SOME tyrow => checkTyRow tyrow
	in
	    U
	end

      | checkTy(CONTy(I, tyseq, longtycon)) =
	let
	    val Tyseq(_,tys) = tyseq
	    val Us = List.map checkTy tys
	in
	    List.foldl TyVarSet.union TyVarSet.empty Us
	end

      | checkTy(ARROWTy(I, ty, ty')) =
	let
	    val U  = checkTy ty
	    val U' = checkTy ty'
	in
	    TyVarSet.union(U, U')
	end

      | checkTy(PACKTy(I, longsigid)) =
	(* [RFC: First-class modules] *)
	    TyVarSet.empty

      | checkTy(PARTy(I, ty)) =
	let
	    val U = checkTy ty
	in
	    U
	end


    (* Type-expression Rows *)

    (* [RFC: Record extension] *)
    and checkTyRow(DOTSTyRow(I, ty)) =
	let
	    val U = checkTy ty
	in
	    U
	end

      | checkTyRow(FIELDTyRow(I, lab, ty, tyrow_opt)) =
	let
	    val U  = checkTy ty
	    val U' = case tyrow_opt
		       of NONE       => TyVarSet.empty
			| SOME tyrow => checkTyRow tyrow
	in
	    (* [RFC: Record extension] *)
	    TyVarSet.union(U, U')
	end



    (* Build tentative VE from LHSs of recursive valbind *)

    and lhsRecValBind(C, ValBind(I, pat, exp, valbind_opt)) =
	let
	    val VE  = lhsRecValBindPat(C, pat)
	    val VE' = case valbind_opt
			of NONE         => VIdMap.empty
			 | SOME valbind => lhsRecValBind(C, valbind)
	in
	    case exp
	      of FNExp _ => VIdMap.unionWith #2 (VE,VE')
	       | _ =>
		(* [Section 2.9, 4th bullet] *)
		error(I, "illegal expression within recursive value binding")
	end

	(* Removed RECValBind [RFC: Simplified recursive value bindings] *)

    and lhsRecValBindPat(C, ATPat(I, atpat)) =
	    lhsRecValBindAtPat(C, atpat)

      | lhsRecValBindPat(C, CONPat(I, _, longvid, atpat)) =
	    lhsRecValBindAtPat(C, atpat)

      | lhsRecValBindPat(C, COLONPat(I, pat, ty)) =
	    lhsRecValBindPat(C, pat)

      (* [RFC: Conjunctive patterns] *)
      | lhsRecValBindPat(C, ASPat(I, pat1, pat2)) =
	    VIdMap.unionWith #2 (lhsRecValBindPat(C, pat1),
			         lhsRecValBindPat(C, pat2))

      (* [RFC: Disjunctive patterns] *)
      | lhsRecValBindPat(C, BARPat(I, pat1, pat2)) =
	    VIdMap.unionWith #2 (lhsRecValBindPat(C, pat1),
			         lhsRecValBindPat(C, pat2))

      (* [RFC: Nested matches] *)
      | lhsRecValBindPat(C, WITHPat(I, pat1, pat2, exp)) =
	    VIdMap.unionWith #2 (lhsRecValBindPat(C, pat1),
			         lhsRecValBindPat(C, pat2))

    and lhsRecValBindAtPat(C, WILDCARDAtPat(I)) =
	    VIdMap.empty

      | lhsRecValBindAtPat(C, SCONAtPat(I, scon)) =
	    VIdMap.empty

      | lhsRecValBindAtPat(C, IDAtPat(I, _, longvid)) =
	(* [RFC: Simplified recursive value bindings] *)
	let
	    val (strids, vid) = LongVId.explode longvid
	in
	    if List.null strids andalso
	       ( case findLongVId(C, longvid)
		   of NONE    => true
		    | SOME is => is = IdStatus.v )
	    then
		VIdMap.singleton(vid, IdStatus.v)
	    else
		VIdMap.empty
	end

      | lhsRecValBindAtPat(C, RECORDAtPat(I, patrow_opt)) =
	   (case patrow_opt
	      of NONE        => VIdMap.empty
	       | SOME patrow => lhsRecValBindPatRow(C, patrow)
	   )

      | lhsRecValBindAtPat(C, PARAtPat(I, pat)) =
	    lhsRecValBindPat(C, pat)

    (* [RFC: Record extension] *)
    and lhsRecValBindPatRow(C, DOTSPatRow(I, pat)) =
	let
	    val VE = lhsRecValBindPat(C, pat)
	in
	    VE
	end

      | lhsRecValBindPatRow(C, FIELDPatRow(I, lab, pat, patrow_opt)) =
	let
	    val VE = lhsRecValBindPat(C, pat)
	in
	    case patrow_opt
	      of NONE        => VE
	       | SOME patrow =>
		 VIdMap.unionWith #2 (VE, lhsRecValBindPatRow(C, patrow))
	end
end;
