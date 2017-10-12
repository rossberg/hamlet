(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules validoration
 *
 * Definition, Sections 5.7 and 3.5
 *
 * Notes:
 *   - To implement the 3rd restriction in 4.11 some valid functions are
 *     passed an additional boolean argument to recognise being on the toplevel.
 *   - Another bug in the Definition -- rules 64 and 78 need additional
 *     side conditions to ensure well-formedness of the constructed realisation:
 *     (64) t in TyName^(k)
 *     (78) t_i in TyName^(k), i = 1..n
 *)

structure ValidModule : VALID_MODULE =
struct
    (* Import *)

    open SyntaxModule
    open Annotation
    open StaticObjectsModule
    open StaticObjectsCore
    open Error


    (* Helpers for basis modification *)

    val plus    = StaticBasis.plus
    val plusT   = StaticBasis.plusT
    val oplusSE = StaticBasis.oplusSE
    val oplusG  = StaticBasis.oplusG
    val oplusF  = StaticBasis.oplusF
    val oplusE  = StaticBasis.oplusE

    infix plus plusT oplusG oplusF oplusE oplusSE


    (* Validation helpers *)

    infix 0 |->
    infix 1 ? ??
    infix 2 @@

    datatype check_result = OK | WARNING of string | ERROR of string

    fun both(result1, result2) =
	case (result1, result2) of
	  (OK, OK) => OK
	| (ERROR s, _) => ERROR s
	| (_, ERROR s) => ERROR s
	| (WARNING s, _) => WARNING s
	| (_, WARNING s) => WARNING s

    fun x ? (predicate@@A) =
	case !(#attr A) of
	  NONE => error(#loc A, "missing annotation")
	| SOME attr =>
	case predicate(x, attr) of
	  OK => attr
	| WARNING s  => (warning(#loc A, s); attr)
	| ERROR s => error(#loc A, s)

    fun  NONE    ?? (predicate@@A) = NONE
      | (SOME x) ?? (predicate@@A) = SOME(x ? predicate@@A)

    fun (x@@A) |-> y = (x, y)

    fun isTypeScheme(sigma, sigma') =
	if TypeScheme.equals(sigma, sigma') then
	  OK
	else
	  ERROR "mismatch on TypeScheme"

    fun isTypeFcn(theta, theta') =
	if TypeFcn.equals(theta, theta') then
	  OK
	else
	  ERROR "mismatch on TypeFcn"

    fun isEnv(E, E') =
	if not(StampMap.isEmpty(StaticEnv.undetermined E')) then
	  ERROR "undetermined types in Env"
	else if StaticEnv.enriches(E, E') andalso StaticEnv.enriches(E', E) then
	  OK
	else
	(
	  print "Derived:\n";
	  PrettyPrint.output(TextIO.stdOut, PPStaticEnv.ppEnv E, 79);
	  print "\nAnnotated:\n";
	  PrettyPrint.output(TextIO.stdOut, PPStaticEnv.ppEnv E', 79);
	  print "\n";
	  TextIO.flushOut TextIO.stdOut;
	  ERROR "mismatch on Env"
	)

    fun isStrEnv(SE, SE') = isEnv(StaticEnv.fromSE SE, StaticEnv.fromSE SE')
    fun isValEnv(VE, VE') = isEnv(StaticEnv.fromVE VE, StaticEnv.fromVE VE')
    fun isTyEnv(TE, TE')  = isEnv(StaticEnv.fromTE TE, StaticEnv.fromTE TE')
    fun isValEnvAndTyEnv(VEandTE, VEandTE') =
	isEnv(StaticEnv.fromVEandTE VEandTE, StaticEnv.fromVEandTE VEandTE')

    fun isValStr((sigma, is), (sigma', is')) =
	both(
	  isTypeScheme(sigma, sigma'),
	  if is = is' then OK else ERROR "mismatch on IdStatus"
	)

    fun isTyStr((theta, VE), (theta', VE')) =
	both(
	  isTypeFcn(theta, theta'),
	  if StaticEnv.equalsVE(VE, VE') orelse VIdMap.isEmpty VE' then
	    OK
	  else
	    ERROR "mismatch on ValEnv"
	)

    fun isSig(Sigma1, Sigma2) =
	let
	    val (_, E1) = Sig.rename Sigma1
	    val (_, E2) = Sig.rename Sigma2
	in
	    Sig.match(E1, Sigma2);
	    Sig.match(E2, Sigma1);
	    OK
	end handle Sig.Match => ERROR "mismatch on Sig"

    fun renamePhi (T,(E,Sigma)) =
	let
	    val phi' = TyNameSet.foldl
			 (fn(t,phi')=> TyNameMap.insert(phi',t,TyName.rename t))
			 TyNameMap.empty T
	    val phi = TyNameMap.map (TypeFcn.rename o TypeFcn.fromTyName) phi'
	    val T'  = TyNameSet.map (fn t => valOf(TyNameMap.find(phi',t))) T
	    val E'  = StaticEnv.realise phi E
	    val (T,E) = Sig.rename Sigma
	    val Sigma' = (T, StaticEnv.realise phi E)
	in
	    (T',(E',Sigma'))
	end

    fun isFunSig(Phi1, Phi2) =
	let
	    val (T1, (E1, Sigma1)) = renamePhi Phi1
	    val (T2, (E2, Sigma2)) = renamePhi Phi2
	    val (_, phi1) = Sig.match(E2, (T1,E1))
	    val (_, phi2) = Sig.match(E1, (T2,E2))
	in
	    Sig.match(StaticEnv.realise phi1 (#2 Sigma1), Sigma2);
	    Sig.match(StaticEnv.realise phi2 (#2 Sigma2), Sigma1);
	    OK
	end handle Sig.Match => ERROR "mismatch on FunSig"

    fun equalsE(E1, E2) =
	StaticEnv.enriches(E1, E2) andalso StaticEnv.enriches(E2, E1)

    fun equalsF(F1, F2) =
	FunIdMap.numItems F1 = FunIdMap.numItems F2 andalso
	FunIdMap.alli
	    (fn(funid, Phi1) =>
		case FunIdMap.find(F2, funid)
		  of NONE      => false
		   | SOME Phi2 =>
		case isFunSig(Phi1, Phi2)
		  of ERROR _ => false
		   | _       => true
	    )
	    F1

    fun equalsG(G1, G2) =
	SigIdMap.numItems G1 = SigIdMap.numItems G2 andalso
	SigIdMap.alli
	    (fn(sigid, Sigma1) =>
		case SigIdMap.find(G2, sigid)
		  of NONE        => false
		   | SOME Sigma2 =>
		case isSig(Sigma1, Sigma2)
		  of ERROR _ => false
		   | _       => true
	    )
	    G1

    fun isBasis(B as (T,F,G,E), B' as (T',F',G',E')) =
	if not(StampMap.isEmpty(StaticBasis.undetermined B')) then
	  ERROR "undetermined types in Basis"
	else if
	  TyNameSet.equal(T, T') andalso equalsE(E, E') andalso
	  equalsF(F, F') andalso equalsG(G, G')
	then
	  OK
	else
	  ERROR "mismatch on Basis"

    fun isFunEnv(F, F') =
	isBasis(
	  StaticBasis.fromTandF(TyNameSet.empty, F),
	  StaticBasis.fromTandF(TyNameSet.empty, F')
	)
    fun isSigEnv(G, G') =
	isBasis(
	  StaticBasis.fromTandG(TyNameSet.empty, G),
	  StaticBasis.fromTandG(TyNameSet.empty, G')
	)


    fun respectsEqualityValStr ((alphas, ref(FunType(tau, _))), is) =
	    TypeFcn.admitsEquality (alphas, tau)
      | respectsEqualityValStr _ = true

    fun respectsEquality ((alphas,tau), VE) =
	let
	    val t = #2(Type.toConsType tau)
	in
	    if TyName.admitsEquality t then
		TyName.toString t = "ref" orelse
		VIdMap.all respectsEqualityValStr VE
	    else
		true
	end

    fun maximisesEquality TE = TyConMap.all respectsEquality TE


    (* Type variable sequences *)

    fun tyvars(SyntaxCore.Seq(tyvars)@@_) = List.map get tyvars



    (* Inference rules [Section 5.7] *)


    (* Structure Expressions *)

    fun validStrExp(B, STRUCTStrExp(strdec)@@A) =
	(* [Rule 50] *)
	let
	    val E = validStrDec(B, strdec)
	in
	    E
	end ? isEnv@@A

      | validStrExp(B, IDStrExp(longstrid@@A')@@A) =
	(* [Rule 51] *)
	let
	    val E = case StaticBasis.findLongStrId(B, longstrid) ?? isEnv@@A'
		      of SOME E => E
		       | NONE =>
			 errorLongStrId(#loc A', "unknown structure ",longstrid)
	in
	    E
	end ? isEnv@@A

      | validStrExp(B, COLONStrExp(strexp, sigexp)@@A) =
	(* [Rule 52] *)
	let
	    val E      = validStrExp(B, strexp)
	    val Sigma  = validSigExp(B, sigexp)
	    val (E',_) = Sig.match(E, Sigma)
			 handle Sig.Match =>
				error(#loc A,
				      "structure does not match annotation")
	in
	    E'
	end ? isEnv@@A

      | validStrExp(B, SEALStrExp(strexp, sigexp)@@A) =
	(* [Rule 53] *)
	let
	    val E       = validStrExp(B, strexp)
	    val (T',E') = Sig.rename(validSigExp(B, sigexp))
	    val (E'',_) = Sig.match(E, (T',E'))
			  handle Sig.Match =>
				 error(#loc A,
				       "structure does not match annotation")
	in
	    if TyNameSet.isEmpty
		    (TyNameSet.intersection(T', StaticBasis.tynames B)) then
		(* Match against annotation to get the same choice of tynames *)
		#1(Sig.match(valOf(!(#attr A)), (T',E')))
	    else
		(* Side condition is always ensured by renaming. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validStrExp(B, APPStrExp(funid@@A', strexp)@@A) =
	(* [Rule 54] *)
	let
	    val E = validStrExp(B, strexp)
	    val (T1,(E1,(T1',E1'))) =
		      case StaticBasis.findFunId(B, funid) ?? isFunSig@@A'
			of SOME Phi => Phi
			 | NONE     =>
			   errorFunId(#loc A', "unknown functor ", funid)
	    val (E'',phi) = Sig.match(E, (T1,E1))
			    handle Sig.Match =>
				   error(#loc A, "signature mismatch \
				                 \in functor application")
	    val (T',E')   = Sig.rename (T1', StaticEnv.realise phi E1')
	in
	    if TyNameSet.isEmpty
		(TyNameSet.intersection(TyNameSet.union(StaticEnv.tynames E,
							StaticBasis.tynames B),
					T')) then
		(* Match against annotation to get the same choice of tynames *)
		#1(Sig.match(valOf(!(#attr A)), (T',E')))
	    else
		(* Side condition is always ensured by renaming. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validStrExp(B, LETStrExp(strdec, strexp)@@A) =
	(* [Rule 55] *)
	let
	    val E1 = validStrDec(B, strdec)
	    val E2 = validStrExp(B oplusE E1, strexp)
	in
	    E2
	end ? isEnv@@A


    (* Structure-level Declarations *)

    and validStrDec(B, DECStrDec(dec)@@A) =
	(* [Rule 56] *)
	let
	    val E = ValidCore.validDec(StaticBasis.Cof B, dec)
	in
	    E
	end ? isEnv@@A

      | validStrDec(B, STRUCTUREStrDec(strbind)@@A) =
	(* [Rule 57] *)
	let
	    val SE = validStrBind(B, strbind)
	in
	    StaticEnv.fromSE SE
	end ? isEnv@@A

      | validStrDec(B, LOCALStrDec(strdec1, strdec2)@@A) =
	(* [Rule 58] *)
	let
	    val E1 = validStrDec(B, strdec1)
	    val E2 = validStrDec(B oplusE E1, strdec2)
	in
	    E2
	end ? isEnv@@A

      | validStrDec(B, EMPTYStrDec@@A) =
	(* [Rule 59] *)
	let
	in
	    StaticEnv.empty
	end ? isEnv@@A

      | validStrDec(B, SEQStrDec(strdec1, strdec2)@@A) =
	(* [Rule 60] *)
	let
	    val E1 = validStrDec(B, strdec1)
	    val E2 = validStrDec(B oplusE E1, strdec2)
	in
	    StaticEnv.plus(E1,E2)
	end ? isEnv@@A


    (* Structure Bindings *)

    and validStrBind(B, StrBind(strid@@A', strexp, strbind_opt)@@A) =
	(* [Rule 61] *)
	let
	    val E  = validStrExp(B, strexp)
	    val SE = case strbind_opt
		       of NONE         => StrIdMap.empty
		        | SOME strbind =>
			  validStrBind(B plusT StaticEnv.tynames E, strbind)
	in
	    StrIdMap.extend(SE, strid@@A' |-> E ? isEnv@@A')
	end ? isStrEnv@@A


    (* Signature Expressions *)

    and validSigExpE(B, SIGSigExp(spec)@@A) =
	(* [Rule 62] *)
	let
	    val E = validSpec(B, spec)
	in
	    E
	end ? isEnv@@A

      | validSigExpE(B, IDSigExp(sigid@@A')@@A) =
	(* [Rule 63] *)
	let
	    val (T,E) = case StaticBasis.findSigId(B, sigid) ?? isSig@@A'
			  of SOME Sigma => Sig.rename Sigma
			   | NONE =>
			     errorSigId(#loc A', "unknown signature ", sigid)
	in
	    (* Match against annotation to get the same choice of tynames *)
	    #1(Sig.match(valOf(!(#attr A)), (T,E)))
	end ? isEnv@@A

      | validSigExpE(B, WHERETYPESigExp(sigexp, tyvarseq, longtycon@@A', ty)@@A)=
	(* [Rule 64] *)
	let
	    val E      = validSigExpE(B, sigexp)
	    val alphas = tyvars(tyvarseq)
	    val tau    = ValidCore.validTy(StaticBasis.Cof B, ty)
	    val t      = case StaticEnv.findLongTyCon(E,longtycon) ?? isTyStr@@A'
			   of NONE =>
			      errorLongTyCon(#loc A', "unknown type ",longtycon)
			    | SOME(theta,VE) =>
			 case TypeFcn.toTyName theta
			   of NONE =>
			      errorLongTyCon(#loc A', "non-flexible type ",
			                              longtycon)
			    | SOME t => t
	    val  _     = if not(TyNameSet.member(StaticBasis.Tof B, t)) then ()
			 else errorLongTyCon(#loc A', "rigid type ", longtycon)
	    val phi    = TyNameMap.singleton(t, (alphas,tau))
	    val  _     = if not(TyName.admitsEquality t)
			 orelse TypeFcn.admitsEquality (alphas,tau) then () else
			     error(#loc A,
			           "type realisation does not respect equality")
	    val  _     = if TyName.arity t = List.length alphas then () else
			     error(#loc A,
			           "type realisation does not respect arity")
	    val E'     = StaticEnv.realise phi E
	    val  _     = if StaticEnv.isWellFormed E' then () else
			     error(#loc A,
			           "type realisation does not respect datatype")
	in
	    E'
	end ? isEnv@@A

    and validSigExp(B, sigexp) =
	(* [Rule 65] *)
	let
	    val E = validSigExpE(B, sigexp)
	    val T = TyNameSet.difference(StaticEnv.tynames E, StaticBasis.Tof B)
	in
	    (T,E)
	end


    (* Signature Declarations *)

    and validSigDec(B, SigDec(sigbind)@@A) =
	(* [Rule 66] *)
	let
	    val G = validSigBind(B, sigbind)
	in
	    G
	end ? isSigEnv@@A


    (* Signature Bindings *)

    and validSigBind(B, SigBind(sigid@@A', sigexp, sigbind_opt)@@A) =
	(* [Rule 67] *)
	let
	    val Sigma = validSigExp(B, sigexp)
	    val G     = case sigbind_opt
			  of NONE         => SigIdMap.empty
			   | SOME sigbind => validSigBind(B, sigbind)
	in
	    SigIdMap.extend(G, sigid@@A' |-> Sigma ? isSig@@A')
	end ? isSigEnv@@A


    (* Specifications *)

    and validSpec(B, VALSpec(valdesc)@@A) =
	(* [Rule 68] *)
	let
	    val VE = validValDesc(StaticBasis.Cof B, valdesc)
	in
	    StaticEnv.fromVE(StaticEnv.Clos VE)
	end ? isEnv@@A

      | validSpec(B, TYPESpec(typdesc)@@A) =
	(* [Rule 69] *)
	let
	    val TE = validTypDesc false (StaticBasis.Cof B, typdesc)
	in
	    if List.all (fn(t,VE) =>
			 not(TyName.admitsEquality(valOf(TypeFcn.toTyName t))))
			(TyConMap.listItems TE) then
		StaticEnv.fromTE TE
	    else
		(* Side condition is always ensured by validTypDesc false. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validSpec(B, EQTYPESpec(typdesc)@@A) =
	(* [Rule 70] *)
	let
	    val TE = validTypDesc true (StaticBasis.Cof B, typdesc)
	in
	    if List.all (fn(t,VE) =>
			 TyName.admitsEquality(valOf(TypeFcn.toTyName t)))
			(TyConMap.listItems TE) then
		StaticEnv.fromTE TE
	    else
		(* Side condition is always ensured by validTypDesc true. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validSpec(B, DATATYPESpec(datdesc)@@A) =
	(* [Rule 71] *)
	let
	    val Env(_, TE', _) = valOf(!(#attr A))
	    val (VE,TE) = validDatDesc(Context.oplusTE(StaticBasis.Cof B,TE'),
				       datdesc)
	in
	    if maximisesEquality TE then () else
		error(#loc A, "non-maximal equality");
	    if List.all (fn(t,VE') =>
			    not(TyNameSet.member(StaticBasis.tynames B,
						 valOf(TypeFcn.toTyName t))))
			(TyConMap.listItems TE) then
		StaticEnv.fromVEandTE(VE,TE)
	    else
		(* Side condition is always ensured by stamping. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validSpec(B, DATATYPE2Spec(tycon@@A'', longtycon@@A')@@A) =
	(* [Rule 72] *)
	let
	    val (theta,VE) = case StaticBasis.findLongTyCon(B, longtycon) ?? isTyStr@@A'
			      of SOME tystr => tystr
			       | NONE =>
				 errorLongTyCon(#loc A', "unknown type ",
				                         longtycon)
	    val  TE        = TyConMap.singleton(tycon@@A'' |-> (theta,VE) ? isTyStr@@A'')
	in
	    StaticEnv.fromVEandTE(VE,TE)
	end ? isEnv@@A

      | validSpec(B, EXCEPTIONSpec(exdesc)@@A) =
	(* [Rule 73] *)
	let
	    val VE = validExDesc(StaticBasis.Cof B, exdesc)
	in
	    StaticEnv.fromVE VE
	end ? isEnv@@A

      | validSpec(B, STRUCTURESpec(strdesc)@@A) =
	(* [Rule 74] *)
	let
	    val SE = validStrDesc(B, strdesc)
	in
	    StaticEnv.fromSE SE
	end ? isEnv@@A

      | validSpec(B, INCLUDESpec(sigexp)@@A) =
	(* [Rule 75] *)
	let
	    val E = validSigExpE(B, sigexp)
	in
	    E
	end ? isEnv@@A

      | validSpec(B, EMPTYSpec@@A) =
	(* [Rule 76] *)
	let
	in
	    StaticEnv.empty
	end ? isEnv@@A

      | validSpec(B, SEQSpec(spec1, spec2)@@A) =
	(* [Rule 77] *)
	let
	    val E1 = validSpec(B, spec1)
	    val E2 = validSpec(B oplusE E1, spec2)
	    val _  = if StaticEnv.disjoint(E1,E2) then () else
		     error(#loc A, "duplicate specifications in signature")
	in
	    StaticEnv.plus(E1,E2)
	end ? isEnv@@A

      | validSpec(B, SHARINGTYPESpec(spec, longtycons)@@A) =
	(* [Rule 78] *)
	let
	    val E  = validSpec(B, spec)
	    val ts =
		List.map
		(fn longtycon@@A' =>
		 case StaticEnv.findLongTyCon(E, longtycon) ?? isTyStr@@A'
		   of NONE =>
			 errorLongTyCon(#loc A', "unknown type ", longtycon)
		    | SOME(theta,VE) =>
		 case TypeFcn.toTyName theta
		   of NONE =>
			 errorLongTyCon(#loc A', "non-flexible type ",longtycon)
		    | SOME t =>
		      if TyNameSet.member(StaticBasis.Tof B, t) then
			 errorLongTyCon(#loc A', "rigid type ", longtycon)
		      else
			 t
		)
		longtycons
	    val t = valOf(List.find (fn t => TyName.admitsEquality t =
				       List.exists TyName.admitsEquality ts) ts)
	    val _ = if List.all (fn ti => TyName.arity ti = TyName.arity t) ts
		    then () else
			(* Implicit in definition of realisation *)
			error(#loc A, "type sharing does not respect arity")
	    val phi = List.foldl
			  (fn(ti, phi) =>
				TyNameMap.insert(phi, ti, TypeFcn.fromTyName t))
			  TyNameMap.empty ts
	in
	    TyName.adjustSpan(t,
			      List.foldl Int.max 0 (List.map TyName.span ts));
	    if TyNameSet.isEmpty
		(TyNameSet.intersection(TyNameSet.fromList ts,
					StaticBasis.tynames B)) then
		StaticEnv.realise phi E
	    else
		(* Side condition is always ensured by stamping. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validSpec(B, SHARINGSpec(spec, longstrids)@@A) =
	(* [Appendix A] *)
	let
	    fun shareFlexibleTyName(t1, t2, phi) =
		let
	    	    val _ = if TyName.arity t1 = TyName.arity t2 then () else
			        (* Implicit in definition of realisation *)
			        error(#loc A, "sharing does not respect arity \
					      \of type " ^ TyName.toString t1)
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
				 errorTyCon(#loc A,
				            "structure contains rigid type ",
					    tycon)
			     else
				 shareFlexibleTyName(t1, t2, phi)
			   | _ =>
			     errorTyCon(#loc A,
			                "structure contains non-flexible type ", 
					tycon)
		    )
		    phi TE1

	    fun shareSE(SE1, SE2, phi) =
		StrIdMap.foldli
		    (fn(strid, E1, phi) =>
			case StrIdMap.find(SE2, strid)
			  of NONE    => phi
			   | SOME E2 => shareE(E1, E2, phi)
		    )
		    phi SE1

	    and shareE(Env(SE1,TE1,VE1), Env(SE2,TE2,VE2), phi) =
		let
		    val phi'  = shareTE(TE1, TE2, phi)
		    val phi'' = shareSE(SE1, SE2, phi')
		in
		    phi''
		end

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

	    val E   = validSpec(B, spec)
	    val Es  = List.map
			(fn longstrid@@A' =>
			 case StaticEnv.findLongStrId(E, longstrid) ?? isEnv@@A'
			   of SOME E' => E'
			    | NONE =>
			      errorLongStrId(#loc A', "unknown structure ",
			                              longstrid)
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
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A


    (* Value Descriptions *)

    and validValDesc(C, ValDesc(vid@@A', ty, valdesc_opt)@@A) =
	(* [Rule 79] *)
	let
	    val tau = ValidCore.validTy(C, ty)
	    val VE  = case valdesc_opt
			of NONE         => VIdMap.empty
			 | SOME valdesc => validValDesc(C, valdesc)
	in
	    VIdMap.extend(VE, vid@@A' |-> (([],tau),IdStatus.v) ? isValStr@@A')
	end ? isValEnv@@A


    (* Type Descriptions *)

    and validTypDesc eq (C, TypDesc(tyvarseq, tycon@@A', typdesc_opt)@@A) =
	(* [Rule 80] *)
	let
	    val alphas = tyvars(tyvarseq)
	    val k      = List.length alphas
	    val t      = case TyNameSet.listItems(
				StaticEnv.tynamesTE(valOf(!(#attr A))))
	                   of [t] => t
	                    | _ =>
			      error(#loc A, "missing or unexpected type names")
	    val TE     = case typdesc_opt
			   of NONE         => TyConMap.empty
			    | SOME typdesc =>
				let
				    val TE = validTypDesc eq (C, typdesc)
				in
				    if TyNameSet.member(StaticEnv.tynamesTE TE,
							t) then
					(* Side condition is always ensured
					 * by stamping. *)
					error(#loc A, "inconsistent type names")
				    else
					TE
				end
	    val tau    = Type.fromConsType (List.map Type.fromTyVar alphas, t)
	in
	    TyConMap.extend(TE, tycon@@A' |-> ((alphas,tau), VIdMap.empty) ? isTyStr@@A')
	end ? isTyEnv@@A


    (* Datatype Descriptions *)

    and validDatDesc(C, DatDesc(tyvarseq, tycon@@A', condesc, datdesc_opt)@@A) =
	(* [Rule 81, part 2] *)
	let
	    val (alphas,tau) = case Context.findTyCon(C, tycon) ?? isTyStr@@A'
				 of SOME(theta,VE) => theta
				  | NONE => (* lhsDatDesc inserted it! *)
				    raise Fail "ValidCore.validDatDesc: \
						\tycon not pre-bound"
	    val VE       = validConDesc(C,tau, condesc)
	    val(VE',TE') = case datdesc_opt
			     of NONE         => ( VIdMap.empty, TyConMap.empty )
			      | SOME datdesc =>
				let
				    val  t = valOf(TypeFcn.toTyName(alphas,tau))
				    val (VE',TE') = validDatDesc(C, datdesc)
				in
				    if List.all (fn(t',VE'') =>
						t <> valOf(TypeFcn.toTyName t'))
					 	(TyConMap.listItems TE') then
					(VE',TE')
				    else
					(* Side condition is always ensured
					 * by stamping. *)
					error(#loc A, "inconsistent type names")
				end
	    val ClosVE   = StaticEnv.Clos VE
	in
	    ( VIdMap.unionWith #2 (ClosVE,VE')
	    , TyConMap.extend(TE', tycon@@A' |-> ((alphas,tau),ClosVE) ? isTyStr@@A')
	    )
	end ? isValEnvAndTyEnv@@A


    (* Constructor Descriptions *)

    and validConDesc(C,tau, ConDesc(vid@@A', ty_opt, condesc_opt)@@A) =
	(* [Rule 82] *)
	let
	    val tau1 = case ty_opt
			 of NONE    => tau
			  | SOME ty =>
			    let
				val tau' = ValidCore.validTy(C, ty)
			    in
			        Type.fromFunType(tau',tau)
			    end
	    val VE   = case condesc_opt
			 of NONE         => VIdMap.empty
			  | SOME condesc => validConDesc(C,tau, condesc)
	in
	    VIdMap.extend(VE, vid@@A' |-> (([],tau1),IdStatus.c) ? isValStr@@A')
	end ? isValEnv@@A


    (* Exception Description *)

    and validExDesc(C, ExDesc(vid@@A', ty_opt, exdesc_opt)@@A) =
	(* [Rule 83] *)
	let
	    val tau1 = case ty_opt
			 of NONE    => InitialStaticEnv.tauExn
			  | SOME ty =>
			    let
				val tau = ValidCore.validTy(C, ty)
				val  _  = if TyVarSet.isEmpty(Type.tyvars tau)
					  then () else
					  error(loc ty, "free type variables in\
						       \ exception description")
			    in
			        Type.fromFunType(tau, InitialStaticEnv.tauExn)
			    end
	    val VE   = case exdesc_opt
			 of NONE        => VIdMap.empty
			  | SOME exdesc => validExDesc(C, exdesc)
	in
	    VIdMap.extend(VE, vid@@A' |-> (([],tau1),IdStatus.e) ? isValStr@@A')
	end ? isValEnv@@A


    (* Structure Descriptions *)

    and validStrDesc(B, StrDesc(strid@@A', sigexp, strdesc_opt)@@A) =
	(* [Rule 84] *)
	let
	    val E  = validSigExpE(B, sigexp)
	    val SE = case strdesc_opt
		       of NONE         => StrIdMap.empty
		        | SOME strdesc =>
			  validStrDesc(B plusT StaticEnv.tynames E, strdesc)
	in
	    StrIdMap.extend(SE, strid@@A' |-> E ? isEnv@@A')
	end ? isStrEnv@@A


    (* Functor Declarations *)

    and validFunDec(B, FunDec(funbind)@@A) =
	(* [Rule 85] *)
	let
	    val F = validFunBind(B, funbind)
	in
	    F
	end ? isFunEnv@@A


    (* Functor Bindings *)

    and validFunBind(B, FunBind(funid@@A'', strid@@A', sigexp, strexp, funbind_opt)@@A) =
	(* [Rule 86] *)
	let
	    val (T,E) = validSigExp(B, sigexp)
	    val  E'   = validStrExp(
			   B oplusSE StrIdMap.singleton(strid@@A' |-> E ? isEnv@@A'),
			   strexp)
	    val T'    = TyNameSet.difference(StaticEnv.tynames E',
				TyNameSet.union(StaticBasis.Tof B, T))
	    val F     = case funbind_opt
			  of NONE         => FunIdMap.empty
			   | SOME funbind => validFunBind(B, funbind)
	in
	    if not(TyNameSet.isEmpty
			(TyNameSet.intersection(T, StaticBasis.tynames B))) then
		(* Side condition is always ensured by stamping. *)
		error(#loc A, "inconsistent type names")
	    else
		FunIdMap.extend(F, funid@@A'' |-> (T,(E,(T',E'))) ? isFunSig@@A'')
	end ? isFunEnv@@A


    (* Top-level Declarations *)

    and validTopDec(B, STRDECTopDec(strdec, topdec_opt)@@A) =
	(* [Rule 87] *)
	let
	    val E   = validStrDec(B, strdec)
	    val B'  = case topdec_opt
			of NONE        => StaticBasis.empty
			 | SOME topdec => validTopDec(B oplusE E, topdec)
	    val B'' = StaticBasis.plus
			(StaticBasis.fromTandE(StaticEnv.tynames E, E), B')
	in
	    if not(TyVarSet.isEmpty(StaticBasis.tyvars B'')) then
		error(#loc A, "free type variables on top-level")
	    else if not(StampMap.isEmpty(StaticBasis.undetermined B'')) then
		error(#loc A, "undetermined types on top-level")
	    else
		B''
	end ? isBasis@@A

      | validTopDec(B, SIGDECTopDec(sigdec, topdec_opt)@@A) =
	(* [Rule 88] *)
	let
	    val G   = validSigDec(B, sigdec)
	    val B'  = case topdec_opt
			of NONE        => StaticBasis.empty
			 | SOME topdec => validTopDec(B oplusG G, topdec)
	    val B'' = StaticBasis.plus
			(StaticBasis.fromTandG(StaticBasis.tynamesG G, G), B')
	in
	    B''
	end ? isBasis@@A

      | validTopDec(B, FUNDECTopDec(fundec, topdec_opt)@@A) =
	(* [Rule 89] *)
	let
	    val F   = validFunDec(B, fundec)
	    val B'  = case topdec_opt
			of NONE        => StaticBasis.empty
			 | SOME topdec => validTopDec(B oplusF F, topdec)
	    val B'' = StaticBasis.plus
			(StaticBasis.fromTandF(StaticBasis.tynamesF F, F), B')
	in
	    if not(TyVarSet.isEmpty(StaticBasis.tyvars B'')) then
		error(#loc A, "free type variables on top-level")
	    else if not(StampMap.isEmpty(StaticBasis.undetermined B'')) then
		error(#loc A, "undetermined types on top-level")
	    else
		B''
	end ? isBasis@@A
end;
