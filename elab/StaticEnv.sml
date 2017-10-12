(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML environments of the static semantics of the core
 *
 * Definition, Sections 4.2, 4.3, 4.8, 4.9, and 5.5
 * + RFC: Views
 * + RFC: Abstype as derived
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *
 * Note:
 *     We call the domain type of value environments ValStr.
 *)

structure StaticEnv :> STATIC_ENV =
struct
    (* Inheritance *)

    structure GenericEnv = GenericEnvFn(open StaticObjectsCore
					type ModStr = Mod
					type SigStr = Sig'
					fun env(Struct E)  = SOME E
					  | env(Functor _) = NONE
					fun unEnv(Env E) = E)
    open GenericEnv


    (* Import *)

    open StaticObjectsCore

    type Realisation = Type.Realisation
    type Sig         = StaticObjectsModule.Sig
    type FunSig      = StaticObjectsModule.FunSig
    exception Fct    = StaticObjectsModule.Fct
    exception Sig    = StaticObjectsModule.Sig


    (* Recursive import *)

    structure Sig =
    struct
	val matches : (Sig * Sig -> bool) ref =
	    ref (fn _ => raise Fail "StaticEnv.Sig.matches")
    end

    structure FunSig =
    struct
	val matches : (FunSig * FunSig -> bool) ref =
	    ref (fn _ => raise Fail "StaticEnv.FunSig.matches")
    end


    (* Type variable and type name set [Section 4.2] *)

    fun collect (empty, union, diffTyNames, collectTypeScheme, collectTypeFcn) =
	let
	    fun collectVs(IdStatus is) = empty
	      | collectVs(TyName t)    = collectTypeFcn(TypeFcn.fromTyName t)

	    fun collectVE(VE : ValEnv) =
		VIdMap.foldl
		    (fn((sigma,vs), S) => union(union(S, collectVs vs),
						collectTypeScheme sigma))
		    empty VE

	    fun collectTE(TE : TyEnv) =
		TyConMap.foldl
		    (fn((theta,VE), S) => union(union(S, collectTypeFcn theta),
						collectVE VE)) empty TE
	    fun collectSE(SE : StrEnv) =
		StrIdMap.foldl (fn(M, S) => union(S, collectM M)) empty SE

	    (* [RFC: Nested signatures] *)
	    and collectG(G : SigEnv) =
		SigIdMap.foldl (fn(Sigma, S) => union(S, collectSigma Sigma))
			       empty G

	    and collect(Env(G,SE,TE,VE)) =
		union(union(union(collectG G, collectSE SE), collectTE TE),
		      collectVE VE)

	    (* [RFC: Higher-order functors] *)
	    and collectM(Struct E) =
		    collect E
	      | collectM(Functor(Fct(T,(M,(T',M'))))) =
		    diffTyNames(union(collectM M,
				      diffTyNames(collectM M', T')), T)
	      | collectM(Functor _) =
		    raise Fail "StaticEnv.collect: invalid functor"

	    (* [RFC: Nested signatures] *)
	    and collectSigma(Sig(T,M)) =
		    diffTyNames(collectM M, T)
	      | collectSigma _ =
		    raise Fail "StaticEnv.collect: invalid signature"
	in
	    (collect, collectG, collectSE, collectTE, collectVE, collectM)
	end

    val (tyvars, tyvarsG, tyvarsSE, tyvarsTE, tyvarsVE, tyvarsM) =
	collect(TyVarSet.empty, TyVarSet.union, #1,
		TypeScheme.tyvars, TypeFcn.tyvars)
    val (tynames, tynamesG, tynamesSE, tynamesTE, tynamesVE, tynamesM) =
	collect(TyNameSet.empty, TyNameSet.union, TyNameSet.difference,
		TypeScheme.tynames, TypeFcn.tynames)
    val (undetermined, _, _, _, _, undeterminedM) =
	collect(StampMap.empty, StampMap.unionWith #2, #1,
		TypeScheme.undetermined, TypeFcn.undetermined)


    (* Well-formedness [Section 4.9; RFC: Views] *)

    fun isWellFormedTyStr (theta,VE) =
	isSome(TypeFcn.toTyName theta) orelse
	case VIdMap.first VE
	  of NONE => true
	   | SOME(sigma, IdStatus _) => false
	   | SOME(sigma, TyName t) =>
		VIdMap.all (fn(sigma, TyName t') => t' = t | _ => false) VE

    fun isWellFormedTE TE =
	TyConMap.all isWellFormedTyStr TE

    fun isWellFormedSE SE =
	StrIdMap.all isWellFormedM SE

    and isWellFormedG G =
	SigIdMap.all isWellFormedSigma G

    and isWellFormed (Env(G,SE,TE,VE)) =
	isWellFormedG G andalso isWellFormedTE TE andalso isWellFormedSE SE

    (* [RFC: Higher-order functors] *)
    and isWellFormedM(Struct E) =
	isWellFormed E
      | isWellFormedM(Functor(Fct(T,(M,(T',M'))))) =
	isWellFormedM M andalso isWellFormedM M'
      | isWellFormedM(Functor _) =
	raise Fail "StaticEnv.isWellFormedM: invalid functor"

    (* [RFC: Nested signatures] *)
    and isWellFormedSigma(Sig(T,M)) =
	isWellFormedM M
      | isWellFormedSigma _ =
	raise Fail "StaticEnv.isWellFormedSigma: invalid signature"


    (* Closure [Section 4.8; RFC: Views] *)

    fun Clos VE =
	VIdMap.map (fn((_,tau), vs) => (TypeScheme.Clos tau, vs)) VE


    (* Realisation [Section 5.2; RFC: Views] *)

    fun realiseVs phi (IdStatus is) = IdStatus is
      | realiseVs phi (TyName t)    =
	case Option.mapPartial TypeFcn.toTyName (TyNameMap.find(phi, t))
	  of SOME t' => TyName t'
	   | NONE    => TyName t

    fun realiseVE phi VE =
	VIdMap.map (fn(sigma,vs) => ( TypeScheme.realise phi sigma,
				      realiseVs phi vs )) VE

    and realiseTE phi TE =
	TyConMap.map (fn(theta,VE) => ( TypeFcn.realise phi theta
				      , realiseVE phi VE
				      )) TE
    and realiseSE phi SE =
	StrIdMap.map (realiseM phi) SE

    (* [RFC: Nested signatures] *)
    and realiseG phi G =
	SigIdMap.map (realiseSigma phi) G

    and realise phi (Env(G,SE,TE,VE)) =
	Env( realiseG phi G
	   , realiseSE phi SE
	   , realiseTE phi TE
	   , realiseVE phi VE
	   )

    (* [RFC: Higher-order functors] *)
    and realiseM phi (Struct E) =
	Struct(realise phi E)
      | realiseM phi (Functor(Fct(T,(M,(T',M'))))) =
	Functor(Fct(T, (realiseM phi M, (T', realiseM phi M'))))
      | realiseM phi (Functor _) =
	raise Fail "StaticEnv.realiseM: invalid functor"

    (* [RFC: Nested signatures] *)
    and realiseSigma phi (Sig(T,M)) =
	Sig(T, realiseM phi M)
      | realiseSigma phi _ =
	raise Fail "StaticEnv.realiseSigma: invalid signature"


    (* Maximise equality of a type environment [Section 4.9; RFC: Views],
     * together with its companion value environment
     *)

    fun respectsEqualityValStr ((alphas, ref(FunType(tau, _))), vs) =
	    TypeFcn.admitsEquality (alphas, tau)
      | respectsEqualityValStr _ = true

    fun respectsEquality ((alphas,tau), VE) =
	let
	    val t = Type.tyname tau
	in
	    if TyName.admitsEquality t then
		TyName.toString t = "ref" orelse
		VIdMap.all respectsEqualityValStr VE
	    else
		true
	end

    fun maximiseEquality(TE,VE) =
	let
	    fun checkTyStr((theta, VE), (phi, change)) =
		if respectsEquality (theta,VE) then ( phi, change ) else
		let
		    val t      = Option.valOf(TypeFcn.toTyName theta)
		    val theta' = TypeFcn.fromTyName(TyName.removeEquality t)
		in
		    ( TyNameMap.insert(phi, t, theta'), true )
		end

	    fun checkTE(TE, phi) =
		let
		    val (phi',change) = TyConMap.foldl checkTyStr (phi,false) TE
		    val TE'           = realiseTE phi' TE
		in
		    if change then checkTE(TE', phi')
			      else (TE', phi')
		end

	    val (TE',phi) = checkTE(TE, TyNameMap.empty)
	in
	    ( TE', realiseVE phi VE )
	end


    (* Disjointness *)

    fun disjoint(Env(G1,SE1,TE1,VE1), Env(G2,SE2,TE2,VE2)) =
	    SigIdMap.disjoint(G1,G2) andalso
	    StrIdMap.disjoint(SE1,SE2) andalso
	    TyConMap.disjoint(TE1,TE2) andalso
	    VIdMap.disjoint(VE1,VE2)


    (* Enrichment [Section 5.5; RFC: Views; RFC: Higher-order functors;
     *                          RFC: Nested signatures] *)

    (* [RFC: Views] *)
    fun equalsVE(VE1,VE2) =
	VIdMap.numItems VE1 = VIdMap.numItems VE2 andalso
	VIdMap.alli
	    (fn(vid, (sigma1,vs1)) =>
		case VIdMap.find(VE2, vid)
		  of NONE             => false
		   | SOME(sigma2,vs2) =>
			TypeScheme.equals(sigma1,sigma2) andalso vs1 = vs2
	    )
	    VE1


    fun enriches(Env(G1,SE1,TE1,VE1), Env(G2,SE2,TE2,VE2)) =
	    enrichesG(G1,G2) andalso
	    enrichesSE(SE1,SE2) andalso
	    enrichesTE(TE1,TE2) andalso
	    enrichesVE(VE1,VE2)

    (* [RFC: Nested signatures] *)
    and enrichesG(G1,G2) =
	SigIdMap.alli
	    (fn(sigid, Sigma2) =>
		case SigIdMap.find(G1, sigid)
		  of NONE        => false
		   | SOME Sigma1 => enrichesSigma(Sigma1,Sigma2)
	    )
	    G2

    and enrichesSE(SE1,SE2) =
	StrIdMap.alli
	    (fn(strid, M2) =>
		case StrIdMap.find(SE1, strid)
		  of NONE    => false
		   | SOME M1 => enrichesM(M1,M2)
	    )
	    SE2

    and enrichesTE(TE1,TE2) =
	TyConMap.alli
	    (fn(tycon, tystr2) =>
		case TyConMap.find(TE1, tycon)
		  of NONE        => false
		   | SOME tystr1 => enrichesTyStr(tystr1,tystr2)
	    )
	    TE2

    and enrichesVE(VE1,VE2) =
	VIdMap.alli
	    (fn(vid, valstr2) =>
		case VIdMap.find(VE1, vid)
		  of NONE         => false
		   | SOME valstr1 => enrichesValStr(valstr1,valstr2)
	    )
	    VE2

    and enrichesTyStr((theta1,VE1), (theta2,VE2)) =
	    TypeFcn.equals(theta1,theta2) andalso
	    ( VIdMap.isEmpty VE2 orelse equalsVE(VE1,VE2) orelse
	      equalsVE(VE1, VIdMap.map (fn(sigma,vs) =>
					  (sigma,IdStatus IdStatus.c)) VE2) )

    and enrichesValStr((sigma1,vs1), (sigma2,vs2)) =
	    TypeScheme.generalises(sigma1,sigma2) andalso
	    enrichesVs(vs1, vs2)

    (* [RFC: Views] *)
    and enrichesVs(IdStatus is1, IdStatus is2)   = IdStatus.generalises(is1,is2)
      | enrichesVs(TyName t1, TyName t2)         = t1 = t2
      | enrichesVs(TyName t,IdStatus IdStatus.v) = true
      | enrichesVs(IdStatus IdStatus.c,TyName t) = true
      | enrichesVs _                             = false

    (* [RFC: Higher-order functors] *)
    and enrichesM(Struct E1, Struct E2) =
	    enriches(E1, E2)
      | enrichesM(Functor(Fct Phi1), Functor(Fct Phi2)) =
	    !FunSig.matches(Phi1, Phi2)
      | enrichesM _ = false

    (* [RFC: Nested signatures] *)
    and enrichesSigma(Sig Sigma1, Sig Sigma2) =
	    !Sig.matches(Sigma1, Sigma2) andalso
	    !Sig.matches(Sigma2, Sigma1)
      | enrichesSigma _ = false
end;
