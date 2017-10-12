(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML signatures
 *
 * Definition, Sections 5.1, 5.3, and 5.6
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: First-class modules
 *)

structure Sig :> SIG =
struct
    (* Import *)

    open StaticObjectsCore
    open StaticObjectsModule

    type Realisation = Type.Realisation


    (* Type variable and type name extraction [Section 4.2] *)

    fun tyvars (T,M)       = StaticEnv.tyvarsM M
    fun tynames (T,M)      = TyNameSet.difference(StaticEnv.tynamesM M, T)
    fun undetermined (T,M) = StaticEnv.undeterminedM M


    (* Alpha Renaming *)

    fun rename (T,M) =
	let
	    val phi' = TyNameSet.foldl
			 (fn(t,phi')=> TyNameMap.insert(phi',t,TyName.rename t))
			 TyNameMap.empty T
	    val phi = TyNameMap.map (TypeFcn.rename o TypeFcn.fromTyName) phi'
	    val T'  = TyNameSet.map (fn t => valOf(TyNameMap.find(phi',t))) T
	    val M'  = StaticEnv.realiseM phi M
	in
	    (T',M')
	end


    (* Realisation *)

    fun realise phi Sigma =
	let
	    val (T,M) = rename Sigma
	in
	    (T, StaticEnv.realiseM phi M)
	end


    (* Matching [Section 5.6; RFC: Views; RFC: Higher-order functors] *)

    exception Match

    fun matchTheta(theta', theta, phi, T) =
	if TypeFcn.arity theta <> TypeFcn.arity theta' then
	    raise Match
	else
	case TypeFcn.toTyName theta
	  of NONE   => phi
	   | SOME t =>
		if not(TyNameSet.member(T, t))
		orelse TyNameMap.inDomain(phi, t) then
		    phi
		else if TyName.admitsEquality t
		andalso not(TypeFcn.admitsEquality theta') then
		    raise Match
		else
		let
		    val phi' = TyNameMap.insert(phi, t, TypeFcn.rename theta')
		in
		    TyNameMap.map (TypeFcn.realise phi') phi'
		end

    (* [RFC: Views] *)
    fun matchVE(VE', VE, phi, T) =
	case VIdMap.first VE
	  of NONE                     => phi
	   | SOME(sigma, IdStatus is) => phi
	   | SOME(sigma, TyName t)    =>
	     if not(TyNameSet.member(T, t))
	     orelse TyNameMap.inDomain(phi, t) then
		 phi
	     else case VIdMap.first VE'
	       of SOME(sigma', TyName t') =>
		  let
		     val phi' = TyNameMap.insert(phi, t, TypeFcn.fromTyName t')
		  in
		     TyNameMap.map (TypeFcn.realise phi') phi'
		  end
		| SOME((alphas,tau), IdStatus IdStatus.c) =>
		  let
		     val t'   = case !tau
				  of ConsType(taus,t') => t'
				   | FunType(tau',ref(ConsType(taus,t'))) => t'
				   | _ => raise Match
		     val phi' = TyNameMap.insert(phi, t, TypeFcn.fromTyName t')
		  in
		     TyNameMap.map (TypeFcn.realise phi') phi'
		  end
		| _ => raise Match

    fun matchTE(TE', TE, phi, T) =
	let
	    fun matchTyStr(tycon, (theta,VE), phi) =
		case TyConMap.find(TE', tycon)
		  of NONE             => raise Match
		   | SOME(theta',VE') =>
		     matchVE(VE', VE, matchTheta(theta', theta, phi, T), T)
	in
	    TyConMap.foldli matchTyStr phi TE
	end

    fun matchSE(SE', SE, phi, T) =
	let
	    (* [RFC: Higher-order functors] *)
	    fun matchMod(strid, M, phi) =
		case StrIdMap.find(SE', strid)
		  of NONE    => raise Match
		   | SOME M' => matchM(M', M, phi, T)
	in
	    StrIdMap.foldli matchMod phi SE
	end

    and matchE(Env(G',SE',TE',VE'), Env(G,SE,TE,VE), phi, T) =
	let
	    val phi1 = matchTE(TE', TE, phi, T)
	    val phi2 = matchSE(SE', SE, phi1, T)
	in
	    phi2
	end

    (* [RFC: Higher-order functors] *)
    and matchM(Struct E', Struct E, phi, T) =
	    matchE(E', E, phi, T)
      | matchM(Functor(Fct Phi'), Functor(Fct Phi), phi, T) =
	    phi
      | matchM(_, _, phi, T) =
	    raise Match

    and match(M', (T,M)) =
	let
	    val phi    = matchM(M', M, TyNameMap.empty, T)
	    val Mminus = StaticEnv.realiseM phi M
	in
	    if StaticEnv.enrichesM(M',Mminus) then
		(Mminus, phi)
	    else
		raise Match
	end

    (* [RFC: Higher-order functors] *)
    fun matches(Sigma1, Sigma2) =
	let
	    val (T1,M1) = rename Sigma1
	in
	    (match(M1, Sigma2); true) handle Match => false
	end


    (* Tie recursive imports *)

    fun unSig(Sig Sigma) = Sigma
      | unSig _          = raise Fail "Sig.unSig: invalid signature"

    val _ = Type.Sig.tyvars := (tyvars o unSig)
    val _ = Type.Sig.tynames := (tynames o unSig)
    val _ = Type.Sig.undetermined := (undetermined o unSig)
    val _ = Type.Sig.realise := (fn phi => Sig o realise phi o unSig)
    val _ = Type.Sig.matches :=
	    (fn (Sigma1,Sigma2) => matches(unSig Sigma1, unSig Sigma2))
    val _ = StaticEnv.Sig.matches := matches
end;
