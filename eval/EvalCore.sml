(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML core evaluation
 *
 * Definition, Sections 6.7 and 6.2
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
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     can be avoided (would really be a pain). Note that the state therefore
 *     never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *     Rules of the form A |- phrase => A'/p therefore turn into
 *     A |- phrase => A'.
 *   - We only pass the state where necessary.
 *)

structure EvalCore : EVAL_CORE =
struct
    (* Import *)

    open GrammarCore
    open DynamicObjectsCore
    open Error


    (* Recursive import *)

    structure EvalModule =
    struct
	val evalStrExp : (State ref * Env * GrammarModule.StrExp -> Mod) ref =
	    ref (fn _ => raise Fail "EvalCore.EvalModule.evalStrExp")
	val evalStrDec : (State ref * Env * StrDec' -> Env) ref =
	    ref (fn _ => raise Fail "EvalCore.EvalModule.evalStrDec")
    end


    (* Helpers for environment modification *)

    val plus        = DynamicEnv.plus
    val plusVE      = DynamicEnv.plusVE
    val plusTE      = DynamicEnv.plusTE
    val plusVEandTE = DynamicEnv.plusVEandTE

    infix plus plusVE plusTE plusVEandTE


    (* Special identifiers *)

    val vid_ref  = VId.fromString "ref"
    val vid_from = VId.fromString "from"
    val vid_to   = VId.fromString "to"


    (* Evaluating special constants [Section 6.2] *)

    fun valSCon(SCon.INT(b, sc, ref t_opt)) =
	  SVal(SVal.INT(Library.intFromString(b, sc, t_opt)))
      | valSCon(SCon.WORD(b, sc, ref t_opt)) =
	  SVal(SVal.WORD(Library.wordFromString(b, sc, t_opt)))
      | valSCon(SCon.CHAR(sc, ref t_opt)) =
	  SVal(SVal.CHAR(Library.charFromString(sc, t_opt)))
      | valSCon(SCon.STRING(sc, ref t_opt)) =
	  SVal(SVal.STRING(Library.stringFromString(sc, t_opt)))
      | valSCon(SCon.REAL(sc, ref t_opt)) =
	  SVal(SVal.REAL(Library.realFromString(sc, t_opt)))


    (* Inference rules [Section 6.7] *)

    (* Atomic Expressions *)

    fun evalAtExp(s,E, SCONAtExp(I, scon)) =
	(* [Rule 90] *)
	(valSCon scon handle Overflow =>
	    error(I, "runtime error: special constant out of range")
	)

      | evalAtExp(s,E, IDAtExp(I, _, longvid)) =
	(* [Rule 91; RFC: Views] *)
	let
	    val (v,vs) = case DynamicEnv.findLongVId(E, longvid)
			   of SOME valstr => valstr
			    | NONE =>
			      errorLongVId(I, "runtime error: \
					      \unknown identifier ", longvid)
	in
	    v
	end

      | evalAtExp(s,E, RECORDAtExp(I, exprow_opt)) =
	(* [Rule 92] *)
	let
	    val r = case exprow_opt
		      of NONE        => LabMap.empty
		       | SOME exprow => evalExpRow(s,E, exprow)
	in
	    Record r
	end

      | evalAtExp(s,E, LETAtExp(I, dec, exp)) =
	(* [Rule 93] *)
	let
	    val E' = evalDec(s,E, dec)
	    val v  = evalExp(s,E plus E', exp)
	in
	    v
	end

      | evalAtExp(s,E, PARAtExp(I, exp)) =
	(* [Rule 94] *)
	let
	    val v = evalExp(s,E, exp)
	in
	    v
	end


    (* Expression Rows *)

    and evalExpRow(s,E, FIELDExpRow(I, lab, exp, exprow_opt)) =
	(* [Rule 95] *)
	let
	    val v = evalExp(s,E, exp)
	    val r = case exprow_opt
		      of NONE        => LabMap.empty
		       | SOME exprow => evalExpRow(s,E, exprow)
	in
	    LabMap.insert(r, lab, v)
	end

      | evalExpRow(s,E, DOTSExpRow(I, exp)) =
	(* [Rule 95a; RFC: Record extension] *)
	let
	    val v = evalExp(s,E, exp)
	in
	    case v
	      of Record r => r
	       | _        => error(I, "runtime type error: record expected")
	end


    (* Expressions *)

    and evalExp(s,E, ATExp(I, atexp)) =
	(* [Rule 96] *)
	let
	    val v = evalAtExp(s,E, atexp)
	in
	    v
	end

      | evalExp(s,E, APPExp(I, exp, atexp)) =
	(* [Rules 97 to 103] *)
	let
	    val v1 = evalExp(s,E, exp)
	    val v  = evalAtExp(s,E, atexp)
	in
	    case v1
	      of VId vid =>
		 if vid = vid_ref then
		     (* [Rule 99] *)
		     let
		         val a = Addr.addr()
		     in
			 s := State.insertAddr(!s, a, v);
			 Addr a
		     end
		 else
		     (* [Rule 97] *)
		     VIdVal (vid,v)

	       | ExVal(ExName en) =>
		 (* [Rule 98] *)
		 ExVal(ExNameVal(en,v))

	       | Assign =>
		 (* [Rule 100] *)
		 (case Val.toPair v
		    of SOME(Addr a, v) =>
			( s := State.insertAddr(!s, a, v)
			; Record LabMap.empty
			)
		     | _ => error(I, "runtime type error: address expected")
		 )

	       | BasVal b =>
		 (* [Rule 101] *)
		 (BasVal.APPLY(b, v) handle BasVal.TypeError s =>
		     error(I, "runtime type error: " ^ s))

	       | FcnClosure(match,E',VE) =>
		 (* [Rule 102] *)
		 (let
		     val v' = evalMatch(s,E' plusVE DynamicEnv.Rec VE, v, match)
		  in
		     v'
		  end
		  handle FAIL =>
		     (* [Rule 103] *)
		     raise Pack(ExName InitialDynamicEnv.enMatch)
		 )
	       | _ =>
		 error(I, "runtime type error: applicative value expected")
	end

      | evalExp(s,E, COLONExp(I, exp, _)) =
	(* Omitted [Section 6.1] *)
	evalExp(s,E, exp)

      | evalExp(s,E, PACKExp(I, longstrid, longsigid)) =
	(* [Rule 103a; RFC: First-class modules] *)
	let
	    val strexp' = GrammarModule.IDStrExp(I, longstrid)
	    val sigexp  = GrammarModule.IDSigExp(I, longsigid)
	    val strexp  = GrammarModule.COLONStrExp(I, strexp', sigexp)

	    val M = !EvalModule.evalStrExp(s,E, strexp)
	in
	    Mod M
	end

      | evalExp(s,E, HANDLEExp(I, exp, match)) =
	(* [Rule 104 to 106] *)
	(let
	    val v = evalExp(s,E, exp)
	 in
	    (* [Rule 104] *)
	    v
	 end
	 handle Pack e =>
	    let
		val v = evalMatch(s,E,ExVal e, match)
	    in
		(* [Rule 105] *)
		v
	    end
	    handle FAIL =>
		(* [Rule 106] *)
		raise Pack e
	)

      | evalExp(s,E, RAISEExp(I, exp)) =
	(* [Rule 107] *)
	let
	    val e = case evalExp(s,E, exp)
		      of ExVal e => e
		       | _ => error(I, "runtime type error: \
				       \exception value expected")
	in
	    raise Pack e
	end

      | evalExp(s,E, FNExp(I, match)) =
	(* [Rule 108] *)
	FcnClosure(match,E,VIdMap.empty)


    (* Matches *)

    and evalMatch(s,E,v, Match(I, mrule, match_opt)) =
	(* [Rules 109 to 111] *)
	let
	    val v' = evalMrule(s,E,v, mrule)
	in
	    (* [Rule 109] *)
	    v'
	end
	handle FAIL =>
	    case match_opt
	      of NONE =>
		 (* [Rule 110] *)
		 raise FAIL

	       | SOME match =>
		 (* [Rule 111] *)
		 let
		     val v' = evalMatch(s,E,v, match)
		 in
		     v'
		 end


    (* Match rules *)

    and evalMrule(s,E,v, Mrule(I, pat, exp)) =
	(* [Rules 112 and 113; Appendix A, Figure 15] *)
	let
	    val VE = evalPat(s,E,v, pat)
	    (* [Rule 112] *)
	    val v' = evalExp(s,E plusVE VE, exp)
	in
	    v'
	end
	(* FAIL on evalPat propagates through [Rule 113] *)


    (* Declarations *)

    and evalDec(s,E, VALDec(I, rec_opt, tyvarseq, valbind)) =
	(* [Rule 114; RFC: Simplified recursive value bindings] *)
	let
	    val VE = evalValBind(s,E, valbind)
	in
	    DynamicEnv.fromVE(if rec_opt = WITHRec then DynamicEnv.Rec VE
			      else VE)
	end

      | evalDec(s,E, TYPEDec(I, typbind)) =
	(* [Rule 115] *)
	let
	    val TE = evalTypBind(typbind)
	in
	    DynamicEnv.fromTE TE
	end

      | evalDec(s,E, DATATYPEDec(I, datbind)) =
	(* [Rule 116] *)
	let
	    val (VE,TE) = evalDatBind(datbind)
	in
	    DynamicEnv.fromVEandTE(VE,TE)
	end

      | evalDec(s,E, VIEWTYPEDec(I, tyvarseq, tycon, _, conbind, dec)) =
	(* [Rule 116a; RFC: Views] *)
	let
	    val VE = evalConBind conbind
	    val E' = evalDec(s,E plusVEandTE (VE, TyConMap.singleton(tycon,VE)),
			     dec)
	    val v_from =
		case DynamicEnv.findVId(E', vid_from)
		  of SOME(v,_) => v
		   | NONE =>
		     errorVId(I, "view does not define function ", vid_from)
	    val v_to =
		case DynamicEnv.findVId(E', vid_to)
		  of SOME(v,_) => v
		   | NONE =>
		     errorVId(I, "view does not define function ", vid_to)
	    val VE' = evalConBindV(s,v_from,v_to, conbind)
	    val TE  = TyConMap.singleton(tycon, VE')
	in
	    DynamicEnv.fromVEandTE(VE',TE)
	end

      | evalDec(s,E, DATATYPE2Dec(I, tycon, longtycon)) =
	(* [Rule 117] *)
	let
	    val VE = case DynamicEnv.findLongTyCon(E, longtycon)
		       of SOME VE => VE
			| NONE =>
			  errorLongTyCon(I, "runtime error: unknown type ",
					    longtycon)
	in
	    DynamicEnv.fromVEandTE(VE, TyConMap.singleton(tycon, VE))
	end

      (* Removed rule 116 [RFC: Abstype as derived] *)

      | evalDec(s,E, EXCEPTIONDec(I, exbind)) =
	(* [Rule 119] *)
	let
	    val VE = evalExBind(s,E, exbind)
	in
	    DynamicEnv.fromVE VE
	end

      | evalDec(s,E, STRDECDec(I, strdec)) =
	(* [Rule 119a; RFC: Local modules] *)
	let
	    val E' = !EvalModule.evalStrDec(s,E, strdec)
	in
	    E'
	end

      | evalDec(s,E, LOCALDec(I, dec1, dec2)) =
	(* [Rule 120] *)
	let
	    val E1 = evalDec(s,E, dec1)
	    val E2 = evalDec(s,E plus E1, dec2)
	in
	    E2
	end

      | evalDec(s,E, OPENDec(I, longstrids)) =
	(* [Rule 121] *)
	let
	    val Es =
		List.map
		    (fn longstrid =>
			case DynamicEnv.findLongStrId(E, longstrid)
			  of SOME(Struct E) => E
			   | SOME(Functor _) =>
			     error(I, "runtime error: opening structure")
			   | NONE =>
			     errorLongStrId(I, "runtime error: unknown \
					       \structure ", longstrid) )
		    longstrids
	in
	    List.foldl DynamicEnv.plus DynamicEnv.empty Es
	end

      | evalDec(s,E, EMPTYDec(I)) =
	(* [Rule 122] *)
	DynamicEnv.empty

      | evalDec(s,E, SEQDec(I, dec1, dec2)) =
	(* [Rule 123] *)
	let
	    val E1 = evalDec(s,E, dec1)
	    val E2 = evalDec(s,E plus E1, dec2)
	in
	    E1 plus E2
	end


    (* Value Bindings *)

    and evalValBind(s,E, ValBind(I, pat, exp, valbind_opt)) =
	(* [Rule 124 and 125] *)
	(let
	    val v   = evalExp(s,E, exp)
	    val VE  = evalPat(s,E,v, pat)
	    (* [Rule 124] *)
	    val VE' = case valbind_opt
			of NONE         => VIdMap.empty
			 | SOME valbind => evalValBind(s,E, valbind)
	 in
	    VIdMap.unionWith #2 (VE, VE')
	 end
	 handle FAIL =>
	    (* [Rule 125] *)
	    raise Pack(ExName InitialDynamicEnv.enBind)
	)

	(* Removed rule 126 [RFC: Simplified recursive value bindings] *)


    (* Type Bindings *)

    and evalTypBind(TypBind(I, tyvarseq, tycon, ty, typbind_opt)) =
	(* [Rule 127] *)
	let
	    val TE = case typbind_opt
		       of NONE         => TyConMap.empty
			| SOME typbind => evalTypBind(typbind)
	in
	    TyConMap.insert(TE, tycon, VIdMap.empty)
	end


    (* Datatype Bindings *)

    and evalDatBind(DatBind(I, tyvarseq, tycon, conbind, datbind_opt)) =
	(* [Rule 128] *)
	let
	    val  VE       = evalConBind(conbind)
	    val (VE',TE') = case datbind_opt
			      of NONE          => ( VIdMap.empty, TyConMap.empty )
			       | SOME datbind' => evalDatBind(datbind')
	in
	    ( VIdMap.unionWith #2 (VE, VE')
	    , TyConMap.insert(TE', tycon, VE)
	    )
	end


    (* Constructor Bindings *)

    and evalConBind(ConBind(I, _, vid, _, conbind_opt)) =
	(* [Rule 129] *)
	let
	    val VE = case conbind_opt
		       of NONE         => VIdMap.empty
			| SOME conbind => evalConBind(conbind)
	in
	    VIdMap.insert(VE, vid, (VId vid, IdStatus IdStatus.c))
	end

    and evalConBindV(s,v_from,v_to, ConBind(I, _, vid, NONE, conbind_opt)) =
	(* [Rule 129a; RFC: Views] *)
	let
	    val vid' = VId.invent()
	    val v = evalExp(s,DynamicEnv.fromVE(VIdMap.fromList[
				(vid_from, (v_from, IdStatus IdStatus.v)),
				(vid', (VId vid, IdStatus IdStatus.v))]),
			    APPExp(I, ATExp(I,IDAtExp(I, SANSOp,
						      LongVId.fromId vid_from)),
				      IDAtExp(I,SANSOp,LongVId.fromId vid')))
	    val VE = case conbind_opt
		       of NONE         => VIdMap.empty
			| SOME conbind => evalConBindV(s,v_from,v_to, conbind)
	in
	    VIdMap.insert(VE, vid, (v, Vals(v_to,vid)))
	end

      | evalConBindV(s,v_from,v_to, ConBind(I, _, vid, SOME _, conbind_opt)) =
	(* [Rule 129b; RFC: Views] *)
	let
	    val vid'  = VId.invent()
	    val vid'' = VId.invent()
	    val v     = FcnClosure
		( Match(I,
		    Mrule(I,
		      ATPat(I, IDAtPat(I, SANSOp, LongVId.fromId vid'')),
		      APPExp(I,
			ATExp(I, IDAtExp(I, SANSOp, LongVId.fromId vid_from)),
			PARAtExp(I,
			  APPExp(I,
			    ATExp(I, IDAtExp(I, SANSOp, LongVId.fromId vid')),
			    IDAtExp(I, SANSOp, LongVId.fromId vid''))))), NONE)
		, DynamicEnv.fromVE(VIdMap.fromList[
		      (vid_from, (v_from, IdStatus IdStatus.v)),
		      (vid', (VId vid, IdStatus IdStatus.v))])
		, VIdMap.empty)
	    val VE = case conbind_opt
		       of NONE         => VIdMap.empty
			| SOME conbind => evalConBindV(s,v_from,v_to, conbind)
	in
	    VIdMap.insert(VE, vid, (v, Vals(v_to,vid)))
	end

    (* Exception Bindings *)

    and evalExBind(s,E, NEWExBind(I, _, vid, _, exbind_opt)) =
	(* [Rule 130] *)
	let
	    val en = ExName.exname vid
	    val VE = case exbind_opt
		       of NONE        => VIdMap.empty
			| SOME exbind => evalExBind(s,E, exbind)
	in
	    s := State.insertExName(!s, en);
	    VIdMap.insert(VE, vid, (ExVal(ExName en), IdStatus IdStatus.e))
	end

      | evalExBind(s,E, EQUALExBind(I, _, vid, _, longvid, exbind_opt)) =
	(* [Rule 131] *)
	let
	    val en = case DynamicEnv.findLongVId(E, longvid)
		       of SOME(en, IdStatus IdStatus.e) => en
			| SOME _ =>
			  errorLongVId(I, "runtime error: non-exception \
					  \identifier ", longvid)
			| NONE =>
			  errorLongVId(I, "runtime error: unknown identifier ",
					  longvid)
	    val VE = case exbind_opt
		       of NONE        => VIdMap.empty
			| SOME exbind => evalExBind(s,E, exbind)
	in
	    VIdMap.insert(VE, vid, (en, IdStatus IdStatus.e))
	end


    (* Atomic Patterns *)

    and evalAtPat(s,E,v, WILDCARDAtPat(I)) =
	(* [Rule 132] *)
	VIdMap.empty

      | evalAtPat(s,E,v, SCONAtPat(I, scon)) =
	(* [Rule 133 and 134] *)
	((if Val.equal(v, valSCon scon) then
	   (* [Rule 133] *)
	   VIdMap.empty
	else
	   (* [Rule 134] *)
	   raise FAIL
	) handle Overflow =>
	    error(I, "runtime error: special constant out of range")
	)

      | evalAtPat(s,E,v, IDAtPat(I, _, longvid)) =
	(* [Rule 135 to 137a; RFC: Views] *)
	let
	    val (strids,vid) = LongVId.explode longvid
	in
	    if List.null strids andalso
	       ( case DynamicEnv.findVId(E, vid)
		   of NONE                        => true
		    | SOME(_,IdStatus IdStatus.v) => true
		    | SOME _                      => false )
	    then
		(* [Rule 135] *)
		VIdMap.singleton(vid, (v, IdStatus IdStatus.v))
	    else
		let
		    val (v',vs) =
			case DynamicEnv.findLongVId(E, longvid)
			  of SOME valstr => valstr
			   | NONE => errorLongVId(I, "runtime error: \
						     \unknown constructor ",
						  longvid)
		in
		    case vs
		      of IdStatus IdStatus.v =>
			 errorLongVId(I, "runtime error: non-constructor ",
					 longvid)
		       | IdStatus is =>
			 if Val.equal(v, v') then
			     (* [Rule 136] *)
			     VIdMap.empty
			 else
			     (* [Rule 137] *)
			     raise FAIL
		       | Vals(v_to,vid) =>
			 (* [Rule 137a and 137b; RFC: Views] *)
			 let
			     val vid' = VId.invent()
			     val v''  = evalExp(s,DynamicEnv.fromVE
					  (VIdMap.fromList[
					    (vid_to,(v_to,IdStatus IdStatus.v)),
					    (vid', (v,IdStatus IdStatus.v))]
					  ), APPExp(I,
					       ATExp(I, IDAtExp(I, SANSOp,
						    LongVId.fromId vid_to)),
					       IDAtExp(I, SANSOp,
						    LongVId.fromId vid')))
			 in
			     case v''
			       of VId vid'' =>
				  if vid'' = vid then
				      (* [Rule 137a; RFC: Views] *)
				      VIdMap.empty
				  else
				      (* [Rule 137b; RFC: Views] *)
				      raise FAIL
				| _ =>
				      (* [Rule 137b; RFC: Views] *)
				      raise FAIL
			 end
		end
	end

      | evalAtPat(s,E,v, RECORDAtPat(I, patrow_opt)) =
	(* [Rule 138] *)
	let
	    val r  = case v
		       of Record r => r
		        | _ =>
			  error(I, "runtime type error: record expected")

	    val VE = case patrow_opt
		       of NONE        =>
			  if LabMap.isEmpty r then
			     VIdMap.empty
			  else
			     error(I, "runtime type error: \
				      \empty record expected")

			| SOME patrow =>
			      evalPatRow(s,E,r, patrow)
	in
	    VE
	end

      | evalAtPat(s,E,v, PARAtPat(I, pat)) =
	(* [Rule 139] *)
	let
	    val VE = evalPat(s,E,v, pat)
	in
	    VE
	end


    (* Pattern Rows *)

    and evalPatRow(s,E,r, DOTSPatRow(I, pat)) =
	(* [Rule 140; RFC: Record extension] *)
	let
	    val VE = evalPat(s,E,Record r, pat)
	in
	    VE
	end

      | evalPatRow(s,E,r, FIELDPatRow(I, lab, pat, patrow_opt)) =
	(* [Rule 141 and 142; RFC: Record extension; RFC: Nested matches] *)
	let
	    val v   = case LabMap.find(r, lab)
		        of SOME v => v
		         | _ => errorLab(I, "runtime type error: \
					    \unmatched label ", lab)
	    val VE  = evalPat(s,E,v, pat)
	    (* FAIL on evalPat propagates through [Rule 142] *)
	    (* [Rule 141] *)
	    val VE' =
		case patrow_opt
		  of NONE        => VIdMap.empty
		   | SOME patrow =>
		     evalPatRow(s,E plusVE VE,LabMap.delete(r,lab), patrow)
	in
	    VIdMap.unionWithi #2 (VE, VE')
	end


    (* Patterns *)

    and evalPat(s,E,v, ATPat(I, atpat)) =
	(* [Rule 143] *)
	let
	    val VE = evalAtPat(s,E,v, atpat)
	in
	    VE
	end

      | evalPat(s,E,v, CONPat(I, _, longvid, atpat)) =
	(* [Rules 144 to 148; RFC: Views] *)
	let
	    val (strids,vid) = LongVId.explode longvid
	in
	    if List.null strids andalso vid = vid_ref then
		case v
		  of Addr a =>
		     (* [Rule 148] *)
		     let
			 val v =  case State.findAddr(!s, a)
				    of SOME v => v
				     | NONE   =>
				       raise Fail "EvalCore.evalPat: \
						  \invalid address"
			 val VE = evalAtPat(s,E,v, atpat)
		     in
			 VE
		     end
		   | _ =>
			 error(I, "runtime type error: address expected")
	    else
		case DynamicEnv.findLongVId(E, longvid)
		  of SOME(VId vid, IdStatus IdStatus.c) =>
		     (case v
			of VIdVal(vid',v') =>
			   if vid = vid' then
			       (* [Rule 144] *)
			       let
				   val VE = evalAtPat(s,E,v', atpat)
			       in
				   VE
			       end
			   else
			       (* [Rule 145] *)
			       raise FAIL
			 | _ =>
			       (* [Rule 145] *)
			       raise FAIL
		     )
		   | SOME(ExVal(ExName en), IdStatus IdStatus.e) =>
		     (case v
			of ExVal(ExNameVal(en',v')) =>
	        	   if en = en' then
			       (* [Rule 146] *)
			       let
				   val VE = evalAtPat(s,E,v', atpat)
			       in
				   VE
			       end
			   else
			       (* [Rule 147] *)
			       raise FAIL
			 | _ =>
			       (* [Rule 147] *)
			       raise FAIL
		     )
		   | SOME(v', Vals(v_to,vid)) =>
			 (* [Rule 147a and 147b; RFC: Views] *)
			 let
			     val vid' = VId.invent()
			     val v''' = evalExp(s,DynamicEnv.fromVE
					  (VIdMap.fromList[
					    (vid_to,(v_to,IdStatus IdStatus.v)),
					    (vid', (v,IdStatus IdStatus.v))]
					  ), APPExp(I,
					       ATExp(I, IDAtExp(I, SANSOp,
						    LongVId.fromId vid_to)),
					       IDAtExp(I, SANSOp,
						    LongVId.fromId vid')))
			 in
			     case v'''
			       of VIdVal(vid'',v'') =>
				  if vid'' = vid then
				      (* [Rule 147a; RFC: Views] *)
				      let
					  val VE = evalAtPat(s,E,v'', atpat)
				      in
					  VE
				      end
				  else
				      (* [Rule 147b; RFC: Views] *)
				      raise FAIL
				| _ =>
				      (* [Rule 147b; RFC: Views] *)
				      raise FAIL
			 end
		   | _ =>
			error(I, "runtime type error: constructor expected")
	end

      | evalPat(s,E,v, COLONPat(I, pat, _)) =
	(* Omitted [Section 6.1] *)
	evalPat(s,E,v, pat)

      | evalPat(s,E,v, ASPat(I, pat1, pat2)) =
	(* [Rule 149 and 149a; RFC: Conjunctive patterns; RFC: Nested matches] *)
	let
	    val VE1 = evalPat(s,E,v, pat1)
	    (* FAIL on evalPat propagates through [Rule 149a] *)
	    (* [Rule 149] *)
	    val VE2 = evalPat(s,E plusVE VE1,v, pat2)
	in
	    VIdMap.unionWithi #2 (VE1, VE2)
	end

      | evalPat (s,E,v, BARPat(I, pat1, pat2)) =
	(* [Rules 149b and 149c; RFC: Disjunctive patterns] *)
	(let
	    val VE = evalPat (s,E,v, pat1)
	    (* [Rule 149b] *)
	in
	    VE
	end handle FAIL =>
	(* [Rule 149c] *)
	let
	    val VE = evalPat (s,E,v, pat2)
	in
	    VE
	end)

      | evalPat(s,E,v, WITHPat(I, pat1, pat2, exp)) =
	(* [Rule 149d and 149e; RFC: Nested matches] *)
	let
	    val VE1 = evalPat(s,E,v, pat1)
	    (* FAIL on evalPat propagates through [Rule 149d] *)
	    (* [Rule 149e] *)
	    val v'  = evalExp(s,E plusVE VE1, exp)
	    val VE2 = evalPat(s,E plusVE VE1,v', pat2)
	in
	    VIdMap.unionWithi #2 (VE1, VE2)
	end
end;
