(*
 * (c) Andreas Rossberg 1999-2011
 *
 * Standard ML core validoration
 *
 * Definition, Sections 4.10, 4.11, 4.6, 4.7, 2.9
 *
 * Notes:
 *   - Elaboration also checks the further restrictions [Section 4.11].
 *   - To implement overloading resolution and checks for flexible records,
 *     we accumulate lists of unresolved types at each value declaration.
 *     This requires an additional argument to most valid functions.
 *   - To implement the 3rd restriction in 4.11 some valid functions are
 *     passed an additional boolean argument to recognise being on the toplevel.
 *   - The definition says that overloaded types get defaulted if the
 *     "surrounding text" does not resolve it. It leaves some freedom to
 *     how large this context may be. We choose the innermost value binding.
 *   - The definition states that "the program context" must determine the
 *     exact type of flexible records, but it does not say how large this
 *     context may be either. Again we choose the innermost surrounding value
 *     declaration.
 *)

structure ValidCore : VALID_CORE =
struct
    (* Import *)

    open SyntaxCore
    open Annotation
    open StaticObjectsCore
    open Error


    (* Helpers for context modification *)

    val plus         = StaticEnv.plus
    val plusU        = Context.plusU
    val plusVE       = Context.plusVE
    val oplusE       = Context.oplusE
    val oplusTE      = Context.oplusTE
    val oplusVEandTE = Context.oplusVEandTE

    infix plusU plusVE oplusE oplusTE oplusVEandTE


    (* Validation helpers *)

    infix 0 // |->
    infix 1 ? ?? ?#
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

    fun x ?# (predicate@@A, f) =
	case !(#attr A) of
	  NONE => error(#loc A, "missing annotation")
	| SOME attr =>
	case predicate(x, f attr) of
	  OK => f attr
	| WARNING s  => (warning(#loc A, s); f attr)
	| ERROR s => error(#loc A, s)

    fun  NONE    ?? (predicate@@A) = NONE
      | (SOME x) ?? (predicate@@A) = SOME(x ? predicate@@A)

    fun x // f = f x

    fun (x@@A) |-> y = (x, y)

    fun isType(tau, tau') =
	both(
	  if StampMap.isEmpty(Type.undetermined tau') then OK else
	  (
	    print "Derived:\n";
	    PrettyPrint.output(TextIO.stdOut, PPType.ppType tau, 79);
	    print "\nAnnotated:\n";
	    PrettyPrint.output(TextIO.stdOut, PPType.ppType tau', 79);
	    print "\n";
	    TextIO.flushOut TextIO.stdOut;
	    WARNING "undetermined types in Type"
	  ),
	  (Type.unify(tau, tau'); OK) handle Type.Unify =>
	    (
	      print "Derived:\n";
	      PrettyPrint.output(TextIO.stdOut, PPType.ppType tau, 79);
	      print "\nAnnotated:\n";
	      PrettyPrint.output(TextIO.stdOut, PPType.ppType tau', 79);
	      print "\n";
	      TextIO.flushOut TextIO.stdOut;
	      ERROR "mismatch on Type"
	    )
	)

    fun isTypeRow(rho, rho') =
	isType(Type.fromRowType rho, Type.fromRowType rho')

    fun allowsType(tau, t) = isType(tau, Type.fromConsType([], t))

    fun isTypeScheme(sigma, sigma') =
	if TypeScheme.equals(sigma, sigma') then
	  OK
	else
	(
	  print "Derived:\n";
	  PrettyPrint.output(TextIO.stdOut, PPType.ppTypeScheme sigma, 79);
	  print "\nAnnotated:\n";
	  PrettyPrint.output(TextIO.stdOut, PPType.ppTypeScheme sigma', 79);
	  print "\n";
	  TextIO.flushOut TextIO.stdOut;
	  ERROR "mismatch on TypeScheme"
	)

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

    fun isValEnv(VE, VE') = isEnv(StaticEnv.fromVE VE, StaticEnv.fromVE VE')
    fun isTyEnv(TE, TE')  = isEnv(StaticEnv.fromTE TE, StaticEnv.fromTE TE')
    fun isValEnvAndTyEnv(VEandTE, VEandTE') =
	isEnv(StaticEnv.fromVEandTE VEandTE, StaticEnv.fromVEandTE VEandTE')

    fun isValEnvAndType((VE, tau), (VE', tau')) =
	both(
	  isEnv(StaticEnv.fromVE VE, StaticEnv.fromVE VE'),
	  isType(tau, tau')
	)	  
    fun isValEnvAndTypeRow((VE, rho), (VE', rho')) =
	both(
	  isEnv(StaticEnv.fromVE VE, StaticEnv.fromVE VE'),
	  isTypeRow(rho, rho')
	)

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

    fun tyvars(Seq(tyvars)@@_) = List.map get tyvars



    (* Typing special constants [Section 4.1, Appendix E.1] *)

    fun typeSCon(scon@@A) =
	let
	    val oc  = case scon
	                of SCon.INT _    => StaticLibrary.Int
		         | SCon.WORD _   => StaticLibrary.Word
		         | SCon.CHAR _   => StaticLibrary.Char
		         | SCon.STRING _ => StaticLibrary.String
		         | SCon.REAL _   => StaticLibrary.Real
	in
	    if OverloadingClass.member(oc, valOf(!(#attr A))) then
		Type.fromOverloadingClass oc
	    else
		error(#loc A, "ill-typed special constant")
	end ? allowsType@@A // (fn t => Type.fromConsType([], t))



    (* Inference rules [Section 4.10] *)


    (* Atomic Expressions *)

    fun validAtExp(C, SCONAtExp(scon)@@A) =
	(* [Rule 1] *)
	let
	in
	    typeSCon scon
	end ? (isType@@A)

      | validAtExp(C, IDAtExp(_, longvid@@A')@@A) =
	(* [Rule 2] *)
	let
	    val (sigma,is) = case Context.findLongVId(C, longvid) ?? isValStr@@A'
			       of SOME valstr => valstr
			        | NONE =>
				  errorLongVId(#loc A, "unknown identifier ",
				                       longvid)
	    val (_, tau) = TypeScheme.instance sigma
	in
	    tau
	end ? isType@@A

      | validAtExp(C, RECORDAtExp(exprow_opt)@@A) =
	(* [Rule 3] *)
	let
	    val rho = case exprow_opt
			of NONE        => Type.emptyRow
			 | SOME exprow => validExpRow(C, exprow)
	in
	    Type.fromRowType rho
	end ? isType@@A

      | validAtExp(C, LETAtExp(dec, exp)@@A) =
	(* [Rule 4] *)
	let
	    val E   = validDec(C, dec)
	    val tau = validExp(C oplusE E, exp)
	in
	    if TyNameSet.isSubset(Type.tynames tau, Context.Tof C) then
		tau
	    else
		error(#loc A, "escaping local type name in let expression")
	end ? isType@@A

      | validAtExp(C, PARAtExp(exp)@@A) =
	(* [Rule 5] *)
	let
	    val tau = validExp(C, exp)
	in
	    tau
	end ? isType@@A


    (* Expression Rows *)

    and validExpRow(C, ExpRow(lab@@_, exp, exprow_opt)@@A) =
	(* [Rule 6] *)
	let
	    val tau = validExp(C, exp)
	    val rho = case exprow_opt
			of NONE        => Type.emptyRow
			 | SOME exprow => validExpRow(C, exprow)
	in
	    Type.insertRow(rho, lab, tau)
	end ? isTypeRow@@A


    (* Expressions *)

    and validExp(C, ATExp(atexp)@@A) =
	(* [Rule 7] *)
	let
	    val tau = validAtExp(C, atexp)
	in
	    tau
	end ? isType@@A

      | validExp(C, APPExp(exp, atexp)@@A) =
	(* [Rule 8] *)
	let
	    val tau1 = validExp(C, exp)
	    val tau' = validAtExp(C, atexp)
	    val tau  = Type.guess false
	in
	    Type.unify(tau1, Type.fromFunType(tau',tau))
	    handle Type.Unify => error(#loc A, "type mismatch on application");
	    tau
	end ? isType@@A

      | validExp(C, COLONExp(exp, ty)@@A) =
	(* [Rule 9] *)
	let
	    val tau1 = validExp(C, exp)
	    val tau  = validTy(C, ty)
	in
	    Type.unify(tau1,tau)
	    handle Type.Unify =>
		   error(#loc A, "expression does not match annotation");
	    tau
	end ? isType@@A

      | validExp(C, HANDLEExp(exp, match)@@A) =
	(* [Rule 10] *)
	let
	    val tau1 = validExp(C, exp)
	    val tau2 = validMatch(C, match)
	in
	    Type.unify(Type.fromFunType(InitialStaticEnv.tauExn, tau1), tau2)
	    handle Type.Unify =>
		   error(#loc A, "type mismatch in handler");
	    tau1
	end ? isType@@A

      | validExp(C, RAISEExp(exp)@@A) =
	(* [Rule 11] *)
	let
	    val tau1 = validExp(C, exp)
	in
	    Type.unify(tau1, InitialStaticEnv.tauExn)
	    handle Type.Unify =>
		   error(#loc A, "raised expression is not an exception");
	    Type.guess false
	end ? isType@@A

      | validExp(C, FNExp(match)@@A) =
	(* [Rule 12] *)
	let
	    val tau = validMatch(C, match)
	in
	    tau
	end ? isType@@A


    (* Matches *)

    and validMatch(C, Match(mrule, match_opt)@@A) =
	(* [Rule 13] *)
	let
	    val tau = validMrule(C, mrule)
	in
	    case match_opt
	      of NONE       => tau
	       | SOME match =>
		 let
		     val tau2 = validMatch(C, match)
		 in
		     Type.unify(tau, tau2)
		     handle Type.Unify =>
			    error(#loc A,
			          "type mismatch between different matches");
		     tau
		 end
	end ?# (isType@@A, #tau)


    (* Match rules *)

    and validMrule(C, Mrule(pat, exp)@@A) =
	(* [Rule 14] *)
	let
	    val (VE,tau) = validPat(C, pat)
	    val  tau'    = validExp(C plusVE VE, exp)
	in
	    if TyNameSet.isSubset(StaticEnv.tynamesVE VE, Context.Tof C) then
		Type.fromFunType(tau,tau')
	    else
		(* Side condition is always ensured by stamping. *)
		error(#loc A, "inconsistent type names")
	end ? isType@@A


    (* Declarations *)

    and validDec(C, VALDec(tyvarseq, valbind)@@A) =
	(* [Rule 15] *)
	let
	    val alphas = tyvars(tyvarseq)
	    (* Collect implicitly bound tyvars [Section 4.6] *)
	    val U = TyVarSet.union(TyVarSet.fromList alphas,
			TyVarSet.difference(ScopeTyVars.unguardedTyVars valbind,
					    Context.Uof C))
	    val VE  = validValBind(C plusU U, valbind)
	    val VE' = Clos.Clos (C,valbind) VE
	in
	    if TyVarSet.isEmpty(
			TyVarSet.intersection(U, StaticEnv.tyvarsVE VE')) then
		StaticEnv.fromVE VE'
	    else
		error(#loc A,
		      "some explicit type variables cannot be generalised")
	end ? isEnv@@A

      | validDec(C, TYPEDec(typbind)@@A) =
	(* [Rule 16] *)
	let
	    val TE = validTypBind(C, typbind)
	in
	    StaticEnv.fromTE TE
	end ? isEnv@@A

      | validDec(C, DATATYPEDec(datbind)@@A) =
	(* [Rule 17] *)
	let
	    val Env(_, TE', _) = valOf(!(#attr A))
	    val (VE,TE) = validDatBind(C oplusTE TE', datbind)
	in
	    if maximisesEquality TE then () else
		error(#loc A, "non-maximal equality");
	    if List.all (fn(t,VE') =>
	 		    not(TyNameSet.member(Context.Tof C,
						 valOf(TypeFcn.toTyName t))))
			(TyConMap.listItems TE) then
		StaticEnv.fromVEandTE(VE,TE)
	    else
		(* Side condition is always ensured by stamping. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validDec(C, DATATYPE2Dec(tycon@@A'', longtycon@@A')@@A) =
	(* [Rule 18] *)
	let
	    val (theta,VE) = case Context.findLongTyCon(C, longtycon) ?? isTyStr@@A'
			      of SOME tystr => tystr
			       | NONE =>
				 errorLongTyCon(#loc A', "unknown type ",
				                         longtycon)
	    val  TE        = TyConMap.singleton(tycon@@A'' |-> (theta,VE) ? isTyStr@@A'')
	in
	    StaticEnv.fromVEandTE(VE,TE)
	end ? isEnv@@A

      | validDec(C, ABSTYPEDec(datbind, dec)@@A) =
	(* [Rule 19] *)
	let
	    val Env(_, TE', _) = valOf(!(#attr A))
	    val (VE,TE) = validDatBind(C oplusTE TE', datbind)
	    val    _    = if maximisesEquality TE then () else
			      error(#loc A, "non-maximal equality")
	    val    E    = validDec(C oplusVEandTE (VE,TE), dec)
	in
	    if List.all (fn(t,VE') =>
			    not(TyNameSet.member(Context.Tof C,
						 valOf(TypeFcn.toTyName t))))
			(TyConMap.listItems TE) then
		StaticEnv.Abs(TE,E)
	    else
		(* Side condition is always ensured by stamping. *)
		error(#loc A, "inconsistent type names")
	end ? isEnv@@A

      | validDec(C, EXCEPTIONDec(exbind)@@A) =
	(* [Rule 20] *)
	let
	    val VE = validExBind(C, exbind)
	in
	    StaticEnv.fromVE VE
	end ? isEnv@@A

      | validDec(C, LOCALDec(dec1, dec2)@@A) =
	(* [Rule 21] *)
	let
	    val E1 = validDec(C, dec1)
	    val E2 = validDec(C oplusE E1, dec2)
	in
	    E2
	end ? isEnv@@A

      | validDec(C, OPENDec(longstrids)@@A) =
	(* [Rule 22] *)
	let
	    val Es =
		List.map
		    (fn longstrid@@A' =>
			case Context.findLongStrId(C, longstrid) ?? isEnv@@A'
			  of SOME E => E
			   | NONE =>
			     errorLongStrId(#loc A', "unknown structure ",
			                             longstrid))
		    longstrids
	in
	    List.foldr StaticEnv.plus StaticEnv.empty Es
	end ? isEnv@@A

      | validDec(C, EMPTYDec@@A) =
	(* [Rule 23] *)
	let
	in
	    StaticEnv.empty
	end ? isEnv@@A

      | validDec(C, SEQDec(dec1, dec2)@@A) =
	(* [Rule 24] *)
	let
	    val E1 = validDec(C, dec1)
	    val E2 = validDec(C oplusE E1, dec2)
	in
	    StaticEnv.plus(E1, E2)
	end ? isEnv@@A


    (* Value Bindings *)

    and validValBind(C, PLAINValBind(pat, exp, valbind_opt)@@A) =
	(* [Rule 25] *)
	let
	    val (VE,tau1) = validPat(C, pat)
	    val     tau2  = validExp(C, exp)
	    val  VE'      = case valbind_opt
			      of NONE         => VIdMap.empty
			       | SOME valbind =>
				 validValBind(C, valbind)
	in
	    Type.unify(tau1,tau2)
	    handle Type.Unify =>
		   error(#loc A,
		         "type mismatch between pattern and expression");
	    VIdMap.unionWith #2 (VE,VE')
	end ?# (isValEnv@@A, #VE)

      | validValBind(C, RECValBind(valbind)@@A) =
	(* [Rule 26] *)
	let
	    val {VE = VE1, exhaustive} = valOf(!(#attr A))
	    val VE  = validValBind(C plusVE VE1, valbind)
	in
	    if not(StaticEnv.equalsVE(VE1, VE)) then
		error(#loc A, "type mismatch in recursive binding")
	    else if TyNameSet.isSubset(StaticEnv.tynamesVE VE,
				       Context.Tof C) then
		VE
	    else
		(* Side condition is always ensured by construction. *)
		error(#loc A, "invalid introduction of type names")
	end ?# (isValEnv@@A, #VE)


    (* Type Bindings *)

    and validTypBind(C, TypBind(tyvarseq, tycon@@A', ty, typbind_opt)@@A) =
	(* [Rule 27] *)
	let
	    val alphas = tyvars(tyvarseq)
	    val tau    = validTy(C, ty)
	    val TE     = case typbind_opt
		    	   of NONE         => TyConMap.empty
			    | SOME typbind => validTypBind(C, typbind)
	in
	    TyConMap.extend(TE, tycon@@A' |-> ((alphas,tau),VIdMap.empty) ? isTyStr@@A')
	end ? isTyEnv@@A


    (* Datatype Bindings *)

    and validDatBind(C, DatBind(tyvarseq, tycon@@A', conbind, datbind_opt)@@A) =
	(* [Rule 28, part 2] *)
	let
	    val (alphas,tau) = case Context.findTyCon(C, tycon) ?? isTyStr@@A'
				 of SOME(theta,VE) => theta
				  | NONE => (* lhsDatBind inserted it! *)
				    raise Fail "ValidCore.validDatBind: \
						\tycon not pre-bound"
	    val VE       = validConBind(C,tau, conbind)
	    val(VE',TE') = case datbind_opt
			     of NONE         => ( VIdMap.empty, TyConMap.empty )
			      | SOME datbind =>
				let
				    val  t = valOf(TypeFcn.toTyName(alphas,tau))
				    val (VE',TE') = validDatBind(C, datbind)
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


    (* Constructor Bindings *)

    and validConBind(C,tau, ConBind(_, vid@@A', ty_opt, conbind_opt)@@A) =
	(* [Rule 29] *)
	let
	    val tau1 = case ty_opt
			 of NONE    => tau
			  | SOME ty =>
			    let
				val tau' = validTy(C, ty)
			    in
			        Type.fromFunType(tau',tau)
			    end
	    val VE   = case conbind_opt
			 of NONE         => VIdMap.empty
			  | SOME conbind => validConBind(C,tau, conbind)
	in
	    VIdMap.extend(VE, vid@@A' |-> (([],tau1),IdStatus.c) ? isValStr@@A')
	end ? isValEnv@@A


    (* Exception Bindings *)

    and validExBind(C, NEWExBind(_, vid@@A', ty_opt, exbind_opt)@@A) =
	(* [Rule 30] *)
	let
	    val tau1 = case ty_opt
			 of NONE    => InitialStaticEnv.tauExn
			  | SOME ty =>
			    let
				val tau = validTy(C, ty)
			    in
			        Type.fromFunType(tau, InitialStaticEnv.tauExn)
			    end
	    val VE   = case exbind_opt
			 of NONE        => VIdMap.empty
			  | SOME exbind => validExBind(C, exbind)
	in
	    VIdMap.extend(VE, vid@@A' |-> (([],tau1),IdStatus.e) ? isValStr@@A')
	end ? isValEnv@@A

      | validExBind(C, EQUALExBind(_, vid@@A'', _, longvid@@A', exbind_opt)@@A) =
	(* [Rule 31] *)
	let
	    val tau = case Context.findLongVId(C, longvid) ?? isValStr@@A'
		        of SOME(([],tau),IdStatus.e) => tau
			 | SOME _ =>
			   errorLongVId(#loc A', "non-exception identifier ",
			                         longvid)
			 | NONE =>
			   errorLongVId(#loc A', "unknown identifier ", longvid)
	    val VE  = case exbind_opt
			of NONE        => VIdMap.empty
			 | SOME exbind => validExBind(C, exbind)
	in
	    VIdMap.extend(VE, vid@@A'' |-> (([],tau),IdStatus.e) ? isValStr@@A'')
	end ? isValEnv@@A


    (* Atomic Patterns *)

    and validAtPat(C, WILDCARDAtPat@@A) =
	(* [Rule 32] *)
	let
	in
	    ( VIdMap.empty, Type.guess false )
	end ? isValEnvAndType@@A

      | validAtPat(C, SCONAtPat(scon)@@A) =
	(* [Rule 33] *)
	let
	in
	    ( VIdMap.empty, typeSCon scon )
	end ? isValEnvAndType@@A

      | validAtPat(C, IDAtPat(_, longvid@@A')@@A) =
	(* [Rule 34 and 35] *)
	let
	    val (strids,vid) = LongVId.explode longvid
	in
	    if List.null strids andalso
	       ( case Context.findVId(C, vid)
		   of NONE           => true
		    | SOME(sigma,is) => is = IdStatus.v )
	    then
		(* [Rule 34] *)
		let
		    val tau = Type.guess false
		in
		    ( VIdMap.singleton(vid@@A' |-> (([],tau),IdStatus.v) ? isValStr@@A'), tau )
		end
	    else
		(* [Rule 35] *)
		let
		    val (sigma,is) = case Context.findLongVId(C, longvid) ?? isValStr@@A'
				       of SOME valstr => valstr
				        | NONE =>
					  errorLongVId(#loc A',
					               "unknown constructor ",
						       longvid)
		    val (_, tau)   = TypeScheme.instance sigma
		in
		    if is = IdStatus.v then
			error(#loc A',
			      "non-constructor long identifier in pattern")
		    else case !tau
		      of ConsType _ => 
			 ( VIdMap.empty, tau )
		       | _ => 
			 error(#loc A,"missing constructor argument in pattern")
		end
	end ? isValEnvAndType@@A

      | validAtPat(C, RECORDAtPat(patrow_opt)@@A) =
	(* [Rule 36] *)
	let
	    val (VE,rho) = case patrow_opt
			     of NONE        => (VIdMap.empty, Type.emptyRow)
			      | SOME patrow => validPatRow(C, patrow)
	    val tau = Type.fromRowType rho
	in
	    (VE, tau)
	end ? isValEnvAndType@@A

      | validAtPat(C, PARAtPat(pat)@@A) =
	(* [Rule 37] *)
	let
	    val (VE,tau) = validPat(C, pat)
	in
	    (VE,tau)
	end ? isValEnvAndType@@A


    (* Pattern Rows *)

    and validPatRow(C, DOTSPatRow@@A) =
	(* [Rule 38] *)
	let
	in
	    ( VIdMap.empty, Type.guessRow() )
	end ? isValEnvAndTypeRow@@A

      | validPatRow(C, FIELDPatRow(lab@@_, pat, patrow_opt)@@A) =
	(* [Rule 39] *)
	let
	    val (VE,tau)  = validPat(C, pat)
	    val (VE',rho) = case patrow_opt
			      of NONE        => (VIdMap.empty, Type.emptyRow)
			       | SOME patrow => validPatRow(C, patrow)
	in
	    ( VIdMap.unionWithi (fn(vid,_,_) =>
		    errorVId(#loc A, "duplicate variable ", vid)) (VE,VE')
	    , Type.insertRow(rho, lab, tau)
	    )
	end ? isValEnvAndTypeRow@@A


    (* Patterns *)

    and validPat(C, ATPat(atpat)@@A) =
	(* [Rule 40] *)
	let
	    val (VE,tau) = validAtPat(C, atpat)
	in
	    (VE,tau)
	end ? isValEnvAndType@@A

      | validPat(C, CONPat(_, longvid@@A', atpat)@@A) =
	(* [Rule 41] *)
	let
	    val (sigma,is) = case Context.findLongVId(C, longvid) ?? isValStr@@A'
			       of SOME valstr => valstr
			        | NONE =>
				errorLongVId(#loc A', "unknown constructor ",
				                      longvid)
	    val _          = if is <> IdStatus.v then () else
				errorLongVId(#loc A', "non-constructor ",
				                      longvid)
	    val (tau',tau) = case TypeScheme.instance sigma
			       of (_, ref(FunType(tau',tau))) => (tau', tau)
			        | _ =>
				errorLongVId(#loc A',
				             "misplaced nullary constructor ",
					     longvid)
	    val (VE,tau'2)  = validAtPat(C, atpat)
	in
	    Type.unify(tau',tau'2)
	    handle Type.Unify =>
		   error(#loc A, "type mismatch in constructor pattern");
	    (VE,tau)
	end ? isValEnvAndType@@A

      | validPat(C, COLONPat(pat, ty)@@A) =
	(* [Rule 42] *)
	let
	    val (VE,tau1) = validPat(C, pat)
	    val     tau   = validTy(C, ty)
	in
	    Type.unify(tau1,tau)
	    handle Type.Unify =>
	           error(#loc A, "pattern does not match annotation");
	    (VE,tau)
	end ? isValEnvAndType@@A

      | validPat(C, ASPat(_, vid@@A', ty_opt, pat)@@A) =
	(* [Rule 43] *)
	let
            val _ =
                case Context.findVId(C, vid)
                  of NONE => ()
                   | SOME(sigma,IdStatus.v) => ()
                   | SOME _ =>
                     error(#loc A', "misplaced constructor in pattern")
	    val (VE1,tau1) = validPat(C, pat)
	    val (VE, tau)  =
		case ty_opt
		  of NONE    => (VE1,tau1)
		   | SOME ty =>
		     let
			 val tau = validTy(C, ty)
		     in
			 Type.unify(tau1,tau)
			 handle Type.Unify =>
				error(#loc A,
				      "pattern does not match annotation");
			 (VE1,tau)
		     end
	in
	    if VIdMap.inDomain(VE, vid) then
		errorVId(#loc A', "duplicate variable ", vid)
	    else
		( VIdMap.extend(VE, vid@@A' |-> (([],tau),IdStatus.v) ? isValStr@@A'), tau )
	end ? isValEnvAndType@@A


    (* Type Expressions *)

    and validTy(C, VARTy(tyvar)@@A) =
	(* [Rule 44] *)
	let
	    val alpha@@_ = tyvar
	in
	    Type.fromTyVar alpha
	end ? isType@@A

      | validTy(C, RECORDTy(tyrow_opt)@@A) =
	(* [Rule 45] *)
	let
	    val rho = case tyrow_opt
			of NONE       => Type.emptyRow
			 | SOME tyrow => validTyRow(C, tyrow)
	in
	    Type.fromRowType rho
	end ? isType@@A

      | validTy(C, CONTy(tyseq, longtycon@@A')@@A) =
	(* [Rule 46] *)
	let
	    val Seq(tys)@@_ = tyseq
	    val k           = List.length tys
	    val taus        = List.map (fn ty => validTy(C, ty)) tys
	    val (theta,VE)  =
		case Context.findLongTyCon(C, longtycon) ?? isTyStr@@A'
		  of SOME tystr => tystr
		   | NONE =>
		     errorLongTyCon(#loc A', "unknown type constructor ",
		                             longtycon)
	in
	    TypeFcn.apply(taus, theta)
	    handle TypeFcn.Apply =>
		errorLongTyCon(#loc A, "arity mismatch in type application ",
				       longtycon)
	end ? isType@@A

      | validTy(C, ARROWTy(ty, ty')@@A) =
	(* [Rule 47] *)
	let
	    val tau  = validTy(C, ty)
	    val tau' = validTy(C, ty')
	in
	    Type.fromFunType(tau,tau')
	end ? isType@@A

      | validTy(C, PARTy(ty)@@A) =
	(* [Rule 48] *)
	let
	    val tau = validTy(C, ty)
	in
	    tau
	end ? isType@@A


    (* Type-expression Rows *)

    and validTyRow(C, TyRow(lab@@_, ty, tyrow_opt)@@A) =
	(* [Rule 49] *)
	let
	    val tau = validTy(C, ty)
	    val rho = case tyrow_opt
			of NONE       => Type.emptyRow
			 | SOME tyrow => validTyRow(C, tyrow)
	in
	    Type.insertRow(rho, lab, tau)
	end ? isTypeRow@@A
end;
