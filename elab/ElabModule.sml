(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules elaboration
 *
 * Definition, Sections 5.7 and 3.5
 * + RFC: Semantic fixes
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Notes:
 *   To implement the 3rd restriction in 4.11 some elab functions are
 *   passed an additional boolean argument to recognise being on the toplevel.
 *)

structure ElabModule : ELAB_MODULE =
struct
    (* Import *)

    open GrammarModule
    open StaticObjectsModule
    open StaticObjectsCore
    open Error


    (* Helpers for basis modification *)

    val plus    = StaticBasis.plus
    val plusT   = StaticBasis.plusT
    val oplusSE = StaticBasis.oplusSE
    val oplusG  = StaticBasis.oplusG
    val oplusE  = StaticBasis.oplusE

    infix plus plusT oplusG oplusE oplusSE


    (* Inference rules [Section 5.7] *)


    (* Structure Expressions *)

    fun elabStrExp(C, STRUCTStrExp(I, dec)) =
	(* [Rule 50; RFC: Higher-order functors; RFC: Local modules] *)
	let
	    val E = ElabCore.elabDec false (C, dec)
	in
	    Struct E
	end

      | elabStrExp(C, IDStrExp(I, longstrid)) =
	(* [Rule 51; RFC: Local modules] *)
	let
	    val M = case Context.findLongStrId(C, longstrid)
		      of SOME M => M
		       | NONE =>
			 errorLongStrId(I, "unknown structure ", longstrid)
	in
	    M
	end

      | elabStrExp(C, COLONStrExp(I, strexp, sigexp)) =
	(* [Rule 52; RFC: Local modules] *)
	let
	    val M      = elabStrExp(C, strexp)
	    val Sigma  = elabSigExp(StaticBasis.fromC C, sigexp)
	    val (M',_) = Sig.match(M, Sigma)
			 handle Sig.Match =>
				error(I, "module does not match constraint")
	in
	    M'
	end

      | elabStrExp(C, SEALStrExp(I, strexp, sigexp)) =
	(* [Rule 53; RFC: Local modules] *)
	let
	    val M       = elabStrExp(C, strexp)
	    val (T',M') = Sig.rename(elabSigExp(StaticBasis.fromC C, sigexp))
	    val (M'',_) = Sig.match(M, (T',M'))
			  handle Sig.Match =>
				 error(I, "module does not match constraint")
	in
	    if TyNameSet.isEmpty
		    (TyNameSet.intersection(T', Context.tynames C)) then
		M'
	    else
		(* Side condition is always ensured by renaming. *)
		error(I, "inconsistent type names")
	end

      | elabStrExp(C, UNPACKStrExp(I, atexp, sigexp)) =
	(* [Rule 53a; RFC: First-class modules] *)
	let
	    val tau    = ElabCore.elabAtExp(C, atexp)
	    val Sigma  = elabSigExp(StaticBasis.fromC C, sigexp)
	    val (T,M)  = Sig.rename Sigma
	in
	    Type.unify(tau, Type.fromPackType(Sig Sigma))
	    handle Type.Unify => error(I, "type mismatch on unpack");
	    if TyNameSet.isEmpty
		    (TyNameSet.intersection(T, Context.tynames C)) then
		M
	    else
		(* Side condition is always ensured by renaming. *)
		error(I, "inconsistent type names")
	end

      | elabStrExp(C, APPStrExp(I, strexp1, strexp2)) =
	(* [Rule 54; RFC: Higher-order functors; RFC: Local modules] *)
	let
	    val (T1,(M1,(T1',M1'))) =
		case elabStrExp(C, strexp1)
		  of Functor(Fct Phi) => Phi
		   | _                => error(I, "module is not a functor")
	    val  M        = elabStrExp(C, strexp2)
	    val (M'',phi) = Sig.match(M, (T1,M1))
			    handle Sig.Match =>
				error(I, "module does not match constraint")
	    val (T',M')   = Sig.rename (T1', StaticEnv.realiseM phi M1')
	in
	    if TyNameSet.isEmpty
		(TyNameSet.intersection(TyNameSet.union(StaticEnv.tynamesM M,
							Context.tynames C),
					T')) then
		M'
	    else
		(* Side condition is always ensured by renaming. *)
		error(I, "inconsistent type names")
	end

      | elabStrExp(C, LETStrExp(I, dec, strexp)) =
	(* [Rule 55; RFC: Local modules] *)
	let
	    val E1 = ElabCore.elabDec false (C, dec)
	    val M  = elabStrExp(Context.oplusE(C,E1), strexp)
	in
	    M
	end

      | elabStrExp(C, FCTStrExp(I, strid, sigexp, strexp)) =
	(* [Rule 55a; RFC: Higher-order functors; RFC: Local modules] *)
	let
	    val (T,M) = elabSigExp(StaticBasis.fromC C, sigexp)
	    val  M'   = elabStrExp(
			   Context.oplusSE(C, StrIdMap.singleton(strid, M)),
			   strexp)
	    val  T'   = TyNameSet.difference(StaticEnv.tynamesM M',
				TyNameSet.union(Context.Tof C, T))
	in
	    if not(TyNameSet.isEmpty
			(TyNameSet.intersection(T, Context.tynames C))) then
		(* Side condition is always ensured by stamping. *)
		error(I, "inconsistent type names")
	    else
		Functor(Fct(T,(M,(T',M'))))
	end

      | elabStrExp(C, PARStrExp(I, strexp)) =
	(* [Rule 55b; RFC: Higher-order functors; RFC: Local modules] *)
	let
	    val M = elabStrExp(C, strexp)
	in
	    M
	end


    (* Structure-level Declarations *)

	(* Removed rule 56 [RFC: Local modules] *)

    and elabStrDec(C, STRUCTUREStrDec(I, strbind)) =
	(* [Rule 57; RFC: Local modules] *)
	let
	    val SE = elabStrBind(C, strbind)
	in
	    StaticEnv.fromSE SE
	end

      | elabStrDec(C, SIGNATUREStrDec(I, sigbind)) =
	(* [Rule 57a; RFC: Nested signatures; RFC: Local modules] *)
	let
	    val G = elabSigBind(StaticBasis.fromC C, sigbind)
	in
	    StaticEnv.fromG G
	end

	(* Removed rule 58 [RFC: Local modules] *)

	(* Removed rule 59 [RFC: Local modules] *)

	(* Removed rule 60 [RFC: Local modules] *)


    (* Structure Bindings *)

    and elabStrBind(C, StrBind(I, strid, strexp, strbind_opt)) =
	(* [Rule 61; RFC: Local modules] *)
	let
	    val M  = elabStrExp(C, strexp)
	    val SE = case strbind_opt
		       of NONE         => StrIdMap.empty
		        | SOME strbind =>
			  elabStrBind(Context.plusT(C, StaticEnv.tynamesM M),
				      strbind)
	in
	    StrIdMap.insert(SE, strid, M)
	end


    (* Signature Expressions *)

    and elabSigExpM(B, SIGSigExp(I, spec)) =
	(* [Rule 62; RFC: Higher-order functors] *)
	let
	    val E = elabSpec(B, spec)
	in
	    Struct E
	end

      | elabSigExpM(B, IDSigExp(I, longsigid)) =
	(* [Rule 63; RFC: Nested signatures] *)
	let
	    val (T,M) = case StaticBasis.findLongSigId(B, longsigid)
			  of SOME Sigma => Sig.rename Sigma
			   | NONE => errorLongSigId(I, "unknown signature ",
						    longsigid)
	in
	    M
	end

      | elabSigExpM(B, WHERETYPESigExp(I, sigexp, tyvarseq, longtycon, ty)) =
	(* [Rule 64; RFC: Semantic fixes; RFC: Higher-order functors] *)
	let
	    val E      = case elabSigExpM(B, sigexp)
			   of Struct E  => E
			    | Functor _ =>
			      error(I, "realisation on functor signature")
	    val (U,alphas) = ElabCore.tyvars tyvarseq
	    val tau    = ElabCore.elabTy(StaticBasis.Cof B, ty)
	    val t      = case StaticEnv.findLongTyCon(E,longtycon)
			   of NONE =>
			      errorLongTyCon(I, "unknown type ", longtycon)
			    | SOME(theta,VE) =>
			 case TypeFcn.toTyName theta
			   of NONE =>
			      errorLongTyCon(I, "non-flexible type ", longtycon)
			    | SOME t => t
	    val  _     = if not(TyNameSet.member(StaticBasis.Tof B, t)) then ()
			 else errorLongTyCon(I, "rigid type ", longtycon)
	    val phi    = TyNameMap.singleton(t, (alphas,tau))
	    val  _     = if not(TyName.admitsEquality t)
			 orelse TypeFcn.admitsEquality (alphas,tau) then () else
			  error(I, "type realisation does not respect equality")
	    val  _     = if TyName.arity t = List.length alphas then () else
			  error(I, "type realisation does not respect arity")
	    val E'     = StaticEnv.realise phi E
	    val  _     = if StaticEnv.isWellFormed E' then () else
			  error(I, "type realisation does not respect datatype")
	in
	    Struct E'
	end

      | elabSigExpM(B, FCTSigExp(I, strid, sigexp1, sigexp2)) =
	(* [Rule 64a; RFC: Higher-order functors] *)
	let
	    val (T,M)   = elabSigExp(B, sigexp1)
	    val (T',M') = elabSigExp(
			   B oplusSE StrIdMap.singleton(strid, M),
			   sigexp2)
	in
	    Functor(Fct(T,(M,(T',M'))))
	end

      | elabSigExpM(B, FCTSPECSigExp(I, spec, sigexp2)) =
	(* [Appendix A; RFC: Higher-order functors] *)
	let
	    val E       = elabSpec(B, spec)
	    val (T,M)   = ( TyNameSet.difference(StaticEnv.tynames E,
						 StaticBasis.Tof B),
			    Struct E )
	    val (T',M') = elabSigExp(B oplusE E, sigexp2)
	in
	    Functor(Fct(T,(M,(T',M'))))
	end

      | elabSigExpM(B, PARSigExp(I, sigexp)) =
	(* [Rule 64b; RFC: Higher-order functors] *)
	let
	    val M = elabSigExpM(B, sigexp)
	in
	    M
	end

    and elabSigExp(B, sigexp) =
	(* [Rule 65] *)
	let
	    val M = elabSigExpM(B, sigexp)
	    val T = TyNameSet.difference(StaticEnv.tynamesM M, StaticBasis.Tof B)
	in
	    (T,M)
	end


    (* Removed rule 66 [RFC: Nested signatures] *)


    (* Signature Bindings *)

    and elabSigBind(B, SigBind(I, sigid, sigexp, sigbind_opt)) =
	(* [Rule 67] *)
	let
	    val Sigma = elabSigExp(B, sigexp)
	    val G     = case sigbind_opt
			  of NONE         => SigIdMap.empty
			   | SOME sigbind => elabSigBind(B, sigbind)
	in
	    SigIdMap.insert(G, sigid, Sig Sigma)
	end


    (* Specifications *)

    and elabSpec(B, VALSpec(I, valdesc)) =
	(* [Rule 68] *)
	let
	    val VE = elabValDesc(StaticBasis.Cof B, valdesc)
	in
	    StaticEnv.fromVE(StaticEnv.Clos VE)
	end

      | elabSpec(B, TYPESpec(I, typdesc)) =
	(* [Rule 69] *)
	let
	    val TE = elabTypDesc false (StaticBasis.Cof B, typdesc)
	in
	    if List.all (fn(t,VE) =>
			 not(TyName.admitsEquality(valOf(TypeFcn.toTyName t))))
			(TyConMap.listItems TE) then
		StaticEnv.fromTE TE
	    else
		(* Side condition is always ensured by elabTypDesc false. *)
		error(I, "inconsistent type names")
	end

      | elabSpec(B, EQTYPESpec(I, typdesc)) =
	(* [Rule 70] *)
	let
	    val TE = elabTypDesc true (StaticBasis.Cof B, typdesc)
	in
	    if List.all (fn(t,VE) =>
			 TyName.admitsEquality(valOf(TypeFcn.toTyName t)))
			(TyConMap.listItems TE) then
		StaticEnv.fromTE TE
	    else
		(* Side condition is always ensured by elabTypDesc true. *)
		error(I, "inconsistent type names")
	end

      | elabSpec(B, DATATYPESpec(I, datdesc)) =
	(* [Rule 71] *)
	let
	    val      TE1  = lhsDatDesc datdesc
	    val (VE2,TE2) = elabDatDesc(Context.oplusTE(StaticBasis.Cof B,TE1),
					datdesc)
	    val (TE, VE)  = StaticEnv.maximiseEquality(TE2,VE2)
	in
	    if List.all (fn(t,VE') =>
			    not(TyNameSet.member(StaticBasis.tynames B,
						 valOf(TypeFcn.toTyName t))))
			(TyConMap.listItems TE) then
		StaticEnv.fromVEandTE(VE,TE)
	    else
		(* Side condition is always ensured by stamping. *)
		error(I, "inconsistent type names")
	end

      | elabSpec(B, VIEWTYPESpec(I, tyvarseq, tycon, ty, condesc)) =
	(* [Rule 71a; RFC: Views] *)
	let
	    val (U,alphas) = ElabCore.tyvars tyvarseq
	    val k          = List.length alphas
	    val span       = lhsConDesc condesc
	    val t          = TyName.tyname(TyCon.toString tycon, k, false, span)
	    val tau        = ElabCore.elabTy(StaticBasis.Cof B, ty)
	    val tau'       = Type.fromConsType(List.map Type.fromTyVar alphas,t)
	    val VE         = elabConDesc(StaticBasis.Cof B,tau', condesc)
	    val ClosVE     = StaticEnv.Clos VE
	    val phi        = TyNameMap.singleton(t, (alphas,tau))
	    val VE'        = VIdMap.map (fn(sigma,c) =>
					   (TypeScheme.realise phi sigma,
					    TyName t)) ClosVE
	    val TE         = TyConMap.singleton(tycon, ((alphas,tau),VE'))
	in
	    if TyNameSet.member(StaticBasis.Tof B, t) then
		(* Side condition is always ensured by stamping. *)
		error(I, "inconsistent type names")
	    else
		StaticEnv.fromVEandTE(VE',TE)
	end

      | elabSpec(B, DATATYPE2Spec(I, tycon, longtycon)) =
	(* [Rule 72] *)
	let
	    val (theta,VE) = case StaticBasis.findLongTyCon(B, longtycon)
			      of SOME tystr => tystr
			       | NONE =>
				 errorLongTyCon(I, "unknown type ", longtycon)
	    val  TE        = TyConMap.singleton(tycon, (theta,VE))
	in
	    StaticEnv.fromVEandTE(VE,TE)
	end

      | elabSpec(B, EXCEPTIONSpec(I, exdesc)) =
	(* [Rule 73] *)
	let
	    val VE = elabExDesc(StaticBasis.Cof B, exdesc)
	in
	    StaticEnv.fromVE VE
	end

      | elabSpec(B, STRUCTURESpec(I, strdesc)) =
	(* [Rule 74] *)
	let
	    val SE = elabStrDesc(B, strdesc)
	in
	    StaticEnv.fromSE SE
	end

      | elabSpec(B, SIGNATURESpec(I, sigdesc)) =
	(* [Rule 74a; RFC: Nested signatures] *)
	let
	    val G = elabSigDesc(B, sigdesc)
	in
	    StaticEnv.fromG G
	end

      | elabSpec(B, INCLUDESpec(I, sigexp)) =
	(* [Rule 75] *)
	let
	    val E = case elabSigExpM(B, sigexp)
		      of Struct E  => E
		       | Functor _ =>
			 error(I, "including a functor")
	in
	    E
	end

      | elabSpec(B, EMPTYSpec(I)) =
	(* [Rule 76] *)
	StaticEnv.empty

      | elabSpec(B, SEQSpec(I, spec1, spec2)) =
	(* [Rule 77] *)
	let
	    val E1 = elabSpec(B, spec1)
	    val E2 = elabSpec(B oplusE E1, spec2)
	    val _  = if StaticEnv.disjoint(E1,E2) then () else
		     error(I, "duplicate specifications in signature")
	in
	    StaticEnv.plus(E1,E2)
	end

      | elabSpec(B, SHARINGTYPESpec(I, spec, longtycons)) =
	(* [Rule 78; RFC: Semantic fixes] *)
	let
	    val E  = elabSpec(B, spec)
	    val ts =
		List.map
		(fn longtycon =>
		 case StaticEnv.findLongTyCon(E, longtycon)
		   of NONE =>
			errorLongTyCon(I, "unknown type ", longtycon)
		    | SOME(theta,VE) =>
		 case TypeFcn.toTyName theta
		   of NONE =>
			errorLongTyCon(I, "non-flexible type ", longtycon)
		    | SOME t =>
		      if TyNameSet.member(StaticBasis.Tof B, t) then
			errorLongTyCon(I, "rigid type ", longtycon)
		      else
			t
		)
		longtycons
	    val arity    = TyName.arity(List.hd ts)
	    val equality = List.exists TyName.admitsEquality ts
	    val span  = List.foldl
				(fn(t, span) => Int.max(TyName.span t, span))
				0 ts
	    val t     = TyName.tyname(TyName.toString(List.hd ts),
				      arity, equality, span)
	    val  _    = if List.all (fn ti => TyName.arity ti = arity) ts
			then () else
			  error(I, "type sharing does not respect arity")
	    val phi   = List.foldl
			    (fn(ti, phi) =>
				TyNameMap.insert(phi, ti, TypeFcn.fromTyName t))
			    TyNameMap.empty ts
	in
	    if TyNameSet.isEmpty
		(TyNameSet.intersection(TyNameSet.fromList ts,
					StaticBasis.tynames B)) then
		StaticEnv.realise phi E
	    else
		(* Side condition is always ensured by stamping. *)
		error(I, "inconsistent type names")
	end

      | elabSpec(B, SHARINGSpec(I, spec, longstrids)) =
	(* [Appendix A] *)
	let
	    fun shareFlexibleTyName(t1, t2, phi) =
		let
		    val t = TyName.tyname(TyName.toString t1, TyName.arity t1,
					  TyName.admitsEquality t1 orelse
					      TyName.admitsEquality t2,
					  Int.max(TyName.span t1,
						  TyName.span t2))
		    val theta = TypeFcn.fromTyName t
		in
		    TyNameMap.insert(TyNameMap.insert(phi,
			t1, theta),
			t2, theta)
		end

	    fun shareTE(TE1, TE2, phi) =
		TyConMap.foldli
		    (fn(tycon, (theta1,VE1), phi) =>
			case TyConMap.find(TE2, tycon)
			  of NONE             => phi
			   | SOME(theta2,VE2) =>
			case (TypeFcn.toTyName(TypeFcn.realise phi theta1),
			      TypeFcn.toTyName(TypeFcn.realise phi theta2))
			  of (SOME t1, SOME t2) =>
			     if TyNameSet.member(StaticBasis.Tof B, t1)
			     orelse TyNameSet.member(StaticBasis.Tof B,t2) then
				errorTyCon(I, "structure contains rigid type ",
					      tycon)
			     else
				shareFlexibleTyName(t1, t2, phi)
			   | _ =>
			     errorTyCon(I, "structure contains non-flexible \
					   \type ", tycon)
		    )
		    phi TE1

	    fun shareSE(SE1, SE2, phi) =
		StrIdMap.foldli
		    (fn(strid, M1, phi) =>
			case StrIdMap.find(SE2, strid)
			  of NONE    => phi
			   | SOME M2 => shareM(M1, M2, phi)
		    )
		    phi SE1

	    and shareE(Env(G1,SE1,TE1,VE1), Env(G2,SE2,TE2,VE2), phi) =
		let
		    val phi'  = shareTE(TE1, TE2, phi)
		    val phi'' = shareSE(SE1, SE2, phi')
		in
		    phi''
		end

	    and shareM(Struct E1, Struct E2, phi) = shareE(E1, E2, phi)
	      | shareM(_,         _,         phi) = phi

	    fun share1(E1,   [],   phi) = phi
	      | share1(E1, E2::Es, phi) =
		let
		    val phi' = shareE(E1, E2, phi)
		in
		    share1(E1, Es, phi')
		end

	    fun shareAll( [],   phi) = phi
	      | shareAll(E::Es, phi) =
		let
		    val phi' = share1(E, Es, phi)
		in
		    shareAll(Es, phi')
		end

	    val E   = elabSpec(B, spec)
	    val Es  = List.map
			(fn longstrid =>
			 case StaticEnv.findLongStrId(E, longstrid)
			   of SOME(Struct E') => E'
			    | SOME(Functor _) =>
			      error(I, "sharing functor")
			    | NONE =>
			      errorLongStrId(I, "unknown structure ", longstrid)
			) longstrids
	    val phi = shareAll(Es, TyNameMap.empty)
	in
	    if TyNameSet.isEmpty
		   (TyNameSet.intersection
			(TyNameSet.addList(TyNameSet.empty,
					   TyNameMap.listKeys phi),
			 StaticBasis.tynames B)) then
		StaticEnv.realise phi E
	    else
		(* Side condition is always ensured by stamping. *)
		error(I, "inconsistent type names")
	end


    (* Value Descriptions *)

    and elabValDesc(C, ValDesc(I, vid, ty, valdesc_opt)) =
	(* [Rule 79] *)
	let
	    val tau = ElabCore.elabTy(C, ty)
	    val VE  = case valdesc_opt
			of NONE         => VIdMap.empty
			 | SOME valdesc => elabValDesc(C, valdesc)
	in
	    VIdMap.insert(VE, vid, (([],tau),IdStatus IdStatus.v))
	end


    (* Type Descriptions *)

    and elabTypDesc eq (C, TypDesc(I, tyvarseq, tycon, typdesc_opt)) =
	(* [Rule 80] *)
	let
	    val alphas = #2(ElabCore.tyvars tyvarseq)
	    val k      = List.length alphas
	    val t      = TyName.tyname(TyCon.toString tycon, k, eq, 0)
	    val TE     = case typdesc_opt
			   of NONE         => TyConMap.empty
			    | SOME typdesc => 
				let
				    val TE = elabTypDesc eq (C, typdesc)
				in
				    if TyNameSet.member(StaticEnv.tynamesTE TE,
							t) then
					(* Side condition is always ensured
					 * by stamping. *)
					error(I, "inconsistent type names")
				    else
					TE
				end
	    val tau    = Type.fromConsType (List.map Type.fromTyVar alphas, t)
	in
	    TyConMap.insert(TE, tycon, ((alphas,tau), VIdMap.empty))
	end


    (* Datatype Descriptions *)

    and elabDatDesc(C, DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
	(* [Rule 81, part 2] *)
	let
	    val (U,alphas)   = ElabCore.tyvars tyvarseq
	    val (alphas,tau) = case Context.findTyCon(C, tycon)
				 of SOME(theta,VE) => theta
				  | NONE => (* lhsDatDesc inserted it! *)
				    raise Fail "ElabCore.elabDatDesc: \
						\tycon not pre-bound"
	    val VE       = elabConDesc(C,tau, condesc)
	    val(VE',TE') = case datdesc_opt
			     of NONE         => ( VIdMap.empty, TyConMap.empty )
			      | SOME datdesc =>
				let
				    val  t = valOf(TypeFcn.toTyName(alphas,tau))
				    val (VE',TE') = elabDatDesc(C, datdesc)
				in
				    if List.all (fn(t',VE'') =>
						t <> valOf(TypeFcn.toTyName t'))
					 	(TyConMap.listItems TE') then
					(VE',TE')
				    else
					(* Side condition is always ensured
					 * by stamping. *)
					error(I, "inconsistent type names")
				end
	    val ClosVE   = StaticEnv.Clos VE
	in
	    ( VIdMap.unionWith #2 (ClosVE,VE')
	    , TyConMap.insert(TE', tycon, ((alphas,tau),ClosVE))
	    )
	end


    (* Constructor Descriptions *)

    and elabConDesc(C,tau, ConDesc(I, vid, ty_opt, condesc_opt)) =
	(* [Rule 82] *)
	let
	    val tau1 = case ty_opt
			 of NONE    => tau
			  | SOME ty =>
			    let
				val tau' = ElabCore.elabTy(C, ty)
			    in
			        Type.fromFunType(tau',tau)
			    end
	    val VE   = case condesc_opt
			 of NONE         => VIdMap.empty
			  | SOME condesc => elabConDesc(C,tau, condesc)
	in
	    VIdMap.insert(VE, vid, (([],tau1),IdStatus IdStatus.c))
	end


    (* Exception Description *)

    and elabExDesc(C, ExDesc(I, vid, ty_opt, exdesc_opt)) =
	(* [Rule 83] *)
	let
	    val tau1 = case ty_opt
			 of NONE    => InitialStaticEnv.tauExn
			  | SOME ty =>
			    let
				val tau = ElabCore.elabTy(C, ty)
				val  _  = if TyVarSet.isEmpty(Type.tyvars tau)
					  then () else
					  error(I, "free type variables \
						   \in exception description")
			    in
			        Type.fromFunType(tau, InitialStaticEnv.tauExn)
			    end
	    val VE   = case exdesc_opt
			 of NONE        => VIdMap.empty
			  | SOME exdesc => elabExDesc(C, exdesc)
	in
	    VIdMap.insert(VE, vid, (([],tau1),IdStatus IdStatus.e))
	end


    (* Structure Descriptions *)

    and elabStrDesc(B, StrDesc(I, strid, sigexp, strdesc_opt)) =
	(* [Rule 84] *)
	let
	    val M  = elabSigExpM(B, sigexp)
	    val SE = case strdesc_opt
		       of NONE         => StrIdMap.empty
		        | SOME strdesc =>
			  elabStrDesc(B plusT StaticEnv.tynamesM M, strdesc)
	in
	    StrIdMap.insert(SE, strid, M)
	end


    (* Signature Descriptions *)

    and elabSigDesc(B, SigDesc(I, sigid, sigexp, sigdesc_opt)) =
	(* [Rule 84a; RFC: Nested signatures] *)
	let
	    val Sigma = elabSigExp(B, sigexp)
	    val G     = case sigdesc_opt
			  of NONE         => SigIdMap.empty
			   | SOME sigdesc => elabSigDesc(B, sigdesc)
	in
	    SigIdMap.insert(G, sigid, Sig Sigma)
	end


    (* Removed rule 85 [RFC: Higher-order functors] *)

    (* Removed rule 86 [RFC: Higher-order functors] *)


    (* Top-level Declarations *)

    and elabTopDec(B, TopDec(I, dec)) =
	(* [Rule 87; RFC: Nested signatures; RFC: Local modules] *)
	let
	    val E = ElabCore.elabDec true (StaticBasis.Cof B, dec)
	in
	    if not(TyVarSet.isEmpty(StaticEnv.tyvars E)) then
		error(I, "free type variables on top-level")
	    else if not(StampMap.isEmpty(StaticEnv.undetermined E)) then
		error(I, "undetermined types on top-level")
	    else
		StaticBasis.fromTandE(StaticEnv.tynames E, E)
	end

      (* Removed rule 88 [RFC: Nested signatures] *)

      (* Removed rule 89 [RFC: Higher-order functors] *)



    (* Build tentative TE from LHSs of datdesc *)

    and lhsDatDesc(DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
	(* [Rule 81, part 1] *)
	let
	    val (U,alphas) = ElabCore.tyvars tyvarseq
	    val k          = List.length alphas
	    val span       = lhsConDesc condesc
	    val t          = TyName.tyname(TyCon.toString tycon, k, true, span)
	    val tau        = Type.fromConsType(List.map Type.fromTyVar alphas,t)
	    val TE'        = case datdesc_opt
			       of NONE         => TyConMap.empty
				| SOME datdesc => lhsDatDesc datdesc
	in
	    TyConMap.insert(TE', tycon, ((alphas,tau), VIdMap.empty))
	end

    and lhsConDesc(ConDesc(I, vid, ty_opt, condesc_opt)) =
	case condesc_opt
	  of NONE         => 1
	   | SOME condesc => 1 + lhsConDesc condesc


    (* Tie recursive imports *)

    val _ = ElabCore.ElabModule.elabStrExp := elabStrExp
    val _ = ElabCore.ElabModule.elabStrDec :=
	    (fn(C, StrDec strdec) => elabStrDec(C, strdec)
	      | _ => raise Fail "ElabModule.elabStrDec: invalid declaration")
end;
