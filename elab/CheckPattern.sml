(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML consistency of patterns and matches
 *
 * Definition, Section 4.11
 * + RFC: Conjunctive patterns
 * + RFC: Disjunctive patterns
 * + RFC: Nested matches
 * + RFC: Views
 *
 * Note:
 *   - We represent special constants in a normalised form as strings.
 *   - The requirement to check for irredundancy of matches is a `bug' in the
 *     definition since this cannot be checked in general for two reasons:
 *
 *     (1) There may be (hidden) aliasing of exception constructors.
 *         Consequently, we only detect redundant exception constructors
 *         if they are denoted by the same longvid.
 *
 *     (2) There is no requirement of consistency for constructors in
 *         sharing specifications or type realisations (actually, we
 *         consider this a serious bug). For example,
 *		datatype t1 = A | B
 *		datatype t2 = C
 *		sharing type t1 = t2
 *         is a legal specification. This allows a mix of the constructors
 *         to appear in matches, rendering the terms of irredundancy and
 *         exhaustiveness meaningless. We make no attempt to detect this,
 *         so generated warnings may or may not make sense in that situation.
 *)

structure CheckPattern : CHECK_PATTERN =
struct
    (* Import *)

    type SCon    = SCon.SCon
    type Lab     = Lab.Lab
    type VId     = VId.Id
    type longVId = LongVId.longId
    type Pat     = GrammarCore.Pat
    type Match   = GrammarCore.Match
    type TyName  = TyName.TyName
    type Env     = StaticEnv.Env

    datatype ValStatus = datatype StaticObjectsCore.ValStatus

    open GrammarCore


    (* View consistency *)

    structure LongVIdMap = FinMapFn(type ord_key = LongVId.longId
				    val  compare = LongVId.compare)

    type 'a LabMap     = 'a LabMap.map
    type 'a VIdMap     = 'a VIdMap.map
    type 'a LongVIdMap = 'a LongVIdMap.map

    datatype view =
	  VAR
	| SCON
	| EXCON  of view LongVIdMap
	| CON    of view VIdMap
	| RECORD of view LabMap
	| VIEW   of TyName * view VIdMap

    fun viewMatch(E, view, Match(_, mrule, match_opt)) =
	let
	    val view' = viewMrule(E, view, mrule)
	in
	    case match_opt
	      of NONE       => ()
	       | SOME match => viewMatch(E, view', match)
	end

    and viewMrule(E, view, Mrule(_, pat, _)) =
	    viewPat(E, view, pat)

    and viewAtPat(E, view, WILDCARDAtPat(_)) =
	    view

      | viewAtPat(E, view, SCONAtPat(I, _)) =
	(case view
	   of VAR    => SCON
	    | SCON   => SCON
	    | VIEW _ => Error.error(I, "inconsistent use of view")
	    | _      => raise Fail "CheckPattern.viewAtPat: \
				   \invalid special constant"
	)

      | viewAtPat(E, view, IDAtPat(I, _, longvid)) =
	(case StaticEnv.findLongVId(E, longvid)
	   of NONE =>
		view

	    | SOME(_, IdStatus IdStatus.v) =>
		view

	    | SOME(_, IdStatus IdStatus.e) =>
	      let
		 val m = case view
			   of VAR     => LongVIdMap.empty
			    | EXCON m => m
			    | VIEW _  =>
				Error.error(I, "inconsistent use of view")
			    | _       =>
				raise Fail "CheckPattern.viewAtPat: \
					   \invalid exception constructor"
	      in
		  EXCON(LongVIdMap.insert(m, longvid, VAR))
	      end

	    | SOME(_, IdStatus IdStatus.c) =>
	      let
		 val vid = LongVId.toId longvid
		 val m   = case view
			    of VAR     => VIdMap.empty
			     | CON   m => m
			     | VIEW _  =>
				Error.error(I, "inconsistent use of view")
			     | _       =>
				raise Fail "CheckPattern.viewAtPat: \
					   \invalid constructor"
	      in
		  CON(VIdMap.insert(m, vid, VAR))
	      end

	    | SOME(_, TyName t) =>
	      let
		 val vid = LongVId.toId longvid
		 val m   = case view
			     of VAR        => VIdMap.empty
			      | VIEW(t',m) =>
				if t = t' then m else
				    Error.error(I, "inconsistent use of views")
			      | _          =>
				Error.error(I, "inconsistent use of view")
	      in
		 VIEW(t, VIdMap.insert(m, vid, VAR))
	      end
	)

      | viewAtPat(E, view, RECORDAtPat(I, patrow_opt)) =
	let
	    val m = case view
		      of VAR      => LabMap.empty
		       | RECORD m => m
		       | VIEW _   =>
			 Error.error(I, "inconsistent use of view")
		       | _        => raise Fail "CheckPattern.viewAtPat: \
						\invalid record"
	in
	    RECORD(viewPatRowOpt(E, m, patrow_opt))
	end

      | viewAtPat(E, view, PARAtPat(_, pat)) =
	    viewPat(E, view, pat)

    and viewPat(E, view, ATPat(_, atpat)) =
	    viewAtPat(E, view, atpat)

      | viewPat(E, view, CONPat(I, _, longvid, atpat)) =
	(case StaticEnv.findLongVId(E, longvid)
	   of SOME(_, IdStatus IdStatus.e) =>
	      let
		 val m = case view
			   of VAR     => LongVIdMap.empty
			    | EXCON m => m
			    | VIEW _  =>
				Error.error(I, "inconsistent use of view")
			    | _       =>
				raise Fail "CheckPattern.viewAtPat: \
					   \invalid exception constructor"
		 val view1  = Option.getOpt(LongVIdMap.find(m, longvid), VAR)
		 val view1' = viewAtPat(E, view1, atpat)
		 val m'     = LongVIdMap.insert(m, longvid, view1')
	      in
		  EXCON m'
	      end

	    | SOME(_, IdStatus IdStatus.c) =>
	      let
		 val vid = LongVId.toId longvid
		 val m   = case view
			    of VAR    => VIdMap.empty
			     | CON m  => m
			     | VIEW _ =>
				Error.error(I, "inconsistent use of view")
			     | _      =>
				raise Fail "CheckPattern.viewAtPat: \
					   \invalid constructor"
		 val view1  = Option.getOpt(VIdMap.find(m, vid), VAR)
		 val view1' = viewAtPat(E, view1, atpat)
		 val m'     = VIdMap.insert(m, vid, view1')
	      in
		  CON m'
	      end

	    | SOME(_, TyName t) =>
	      let
		 val vid = LongVId.toId longvid
		 val m   = case view
			     of VAR        => VIdMap.empty
			      | VIEW(t',m) =>
				if t = t' then m else
				    Error.error(I, "inconsistent use of views")
			      | _          =>
				Error.error(I, "inconsistent use of view")
		 val view1  = Option.getOpt(VIdMap.find(m, vid), VAR)
		 val view1' = viewAtPat(E, view1, atpat)
		 val m'     = VIdMap.insert(m, vid, view1')
	      in
		  VIEW(t, m')
	      end

	    | _ => raise Fail "CheckPattern.viewPat: \
			      \invalid constructed pattern"
	)

      | viewPat(E, view, COLONPat(_, pat, _)) =
	    viewPat(E, view, pat)

      (* [RFC: Conjunctive patterns] *)
      | viewPat(E, view, ASPat(_, pat1, pat2)) =
	    viewPat(E, viewPat(E, view, pat1), pat2)

      (* [RFC: Disjunctive patterns] *)
      | viewPat(E, view, BARPat(_, pat1, pat2)) =
	    viewPat(E, viewPat(E, view, pat1), pat2)

      (* [RFC: Nested matches] *)
      | viewPat(E, view, WITHPat(_, pat1, pat2, _)) =
	    viewPat(E, view, pat1)

    and viewPatRowOpt(E, m, NONE) = m

      | viewPatRowOpt(E, m, SOME(FIELDPatRow(_, lab, pat, patrow_opt))) =
	let
	    val view  = Option.getOpt(LabMap.find(m, lab), VAR)
	    val view' = viewPat(E, view, pat)
	    val m'    = LabMap.insert(m, lab, view')
	in
	    viewPatRowOpt(E, m', patrow_opt)
	end

      | viewPatRowOpt(E, m, SOME(DOTSPatRow(_, pat))) =
	let
	    val m' = case viewPat(E, RECORD m, pat)
		       of RECORD m' => m'
		        | _         => raise Fail "CheckPattern.viewPatRowOpt: \
						  \invalid record"
	in
	    LabMap.unionWith #2 (m, m')
	end


    (*
     * Algorithm loosely based on:
     *    Peter Sestoft.
     *         "ML pattern matching compilation and partial evaluation",
     *    in: Dagstuhl Seminar on Partial Evaluation,
     *        Lecture Notes in Computer Science, Springer-Verlag 1996
     *)


    (* Value description *)

    structure SConSet'   = FinSetFn(type ord_key = string
				    val  compare = String.compare)
    structure VIdSet     = FinSetFn(type ord_key = VId.Id
				    val  compare = VId.compare)
    structure LongVIdSet = FinSetFn(type ord_key = LongVId.longId
				    val  compare = LongVId.compare)

    type SCon'         = string
    type SConSet'      = SConSet'.set
    type VIdSet        = VIdSet.set
    type LongVIdSet    = LongVIdSet.set
    type 'a LabMap     = 'a LabMap.map

    datatype description =
	  ANY
	| SCON      of SCon'
	| NOT_SCON  of SConSet'
	| EXCON     of longVId * description option
	| NOT_EXCON of LongVIdSet
	| CON       of VId * description option
	| NOT_CON   of VIdSet
	| REF       of description
	| RECORD    of description LabMap

    datatype context =
	  EXCON'  of context * longVId
	| CON'    of context * VId
	| REF'    of context
	| RECORD' of context * description LabMap * Lab * PatRow option
	| AS1'    of context * Source.info * Pat
	| AS2'    of context * Source.info
	| BAR1'   of context * Source.info * Pat
	| BAR2'   of context * Source.info
	| WITH'   of context * Source.info * bool
	| MATCH'  of Source.info * Match option


    (* Forget knowledge about references *)

    fun invalidateRefs(EXCON(longvid, desc_opt)) =
	    EXCON(longvid, Option.map invalidateRefs desc_opt)
      | invalidateRefs(CON(vid, desc_opt)) =
	    CON(vid, Option.map invalidateRefs desc_opt)
      | invalidateRefs(REF _) =
	    REF ANY
      | invalidateRefs(RECORD descs) =
	    RECORD(LabMap.map invalidateRefs descs)
      | invalidateRefs desc = desc


    (* Normalise special constants *)

    fun normalise(SCon.INT(b, sc, ref t_opt)) =
	  Library.intToString(Library.intFromString(b, sc, t_opt))
      | normalise(SCon.WORD(b, sc, ref t_opt)) =
	  Library.wordToString(Library.wordFromString(b, sc, t_opt))
      | normalise(SCon.CHAR(sc, ref t_opt)) =
	  Library.charToString(Library.charFromString(sc, t_opt))
      | normalise(SCon.STRING(sc, ref t_opt)) =
	  Library.stringToString(Library.stringFromString(sc, t_opt))
      | normalise(SCon.REAL(sc, ref t_opt)) =
	  Library.realToString(Library.realFromString(sc, t_opt))

    fun span scon = case SCon.tyname scon of NONE   => 0
					   | SOME t => Library.span t


    (* Result type for static matching *)

    structure RegionSet = FinSetFn(type ord_key = Source.info
				   val compare  = Source.compare)

    type sets = {matches : RegionSet.set,
		 withs   : RegionSet.set,
		 alts    : RegionSet.set,
		 conjs   : RegionSet.set,
		 reachedMatches : RegionSet.set,
		 reachedWiths   : RegionSet.set,
		 reachedAlts    : RegionSet.set,
		 reachedConjs   : RegionSet.set}

    type result = sets * bool

    val emptySets = {matches = RegionSet.empty,
		     withs   = RegionSet.empty,
		     alts    = RegionSet.empty,
		     conjs   = RegionSet.empty,
		     reachedMatches = RegionSet.empty,
		     reachedWiths   = RegionSet.empty,
		     reachedAlts    = RegionSet.empty,
		     reachedConjs   = RegionSet.empty}

    fun update({matches, withs, alts, conjs,
		reachedMatches, reachedWiths, reachedAlts, reachedConjs}, l, f) =
	let
	    val m = ref matches
	    val w = ref withs
	    val a = ref alts
	    val c = ref conjs
	    val rm = ref reachedMatches
	    val rw = ref reachedWiths
	    val ra = ref reachedAlts
	    val rc = ref reachedConjs
	    val sets = {matches = m, withs = w, alts = a, conjs = c,
			reachedMatches = rm, reachedWiths = rw,
			reachedAlts = ra, reachedConjs = rc}
	    val r = l sets
	in
	    r := f(!r);
	    {matches = !m, withs = !w, alts = !a, conjs = !c,
	     reachedMatches = !rm, reachedWiths = !rw,
	     reachedAlts = !ra, reachedConjs = !rc}
	end

    fun extend(sets, l, I) =
	    update(sets, l, fn R => RegionSet.add(R, I))

    fun branch((sets1, exhaustive1) : result, (sets2, exhaustive2) : result) =
	    ( {matches = RegionSet.union(#matches sets1, #matches sets2),
	       withs   = RegionSet.union(#withs sets1, #withs sets2),
	       alts    = RegionSet.union(#alts sets1, #alts sets2),
	       conjs   = RegionSet.union(#conjs sets1, #conjs sets2),
	       reachedMatches = RegionSet.union(#reachedMatches sets1,
						#reachedMatches sets2),
	       reachedWiths   = RegionSet.union(#reachedWiths sets1,
						#reachedWiths sets2),
	       reachedAlts    = RegionSet.union(#reachedAlts sets1,
						#reachedAlts sets2),
	       reachedConjs   = RegionSet.union(#reachedConjs sets1,
						#reachedConjs sets2)},
	     exhaustive1 andalso exhaustive2 )


    (* Static pattern matching *)

    fun matchMatch(E, desc, Match(_, mrule, match_opt), sets) =
	    matchMrule(E, desc, mrule, match_opt, sets)


    and matchMrule(E, desc, Mrule(I, pat, exp), match_opt, sets) =
	    matchPat(E, desc, pat, MATCH'(I, match_opt), extend(sets, #matches, I))


    and matchAtPat(E, desc, atpat, context, sets) =
	case atpat
	  of WILDCARDAtPat(_) =>
		succeed(E, desc, context, sets)

	   | SCONAtPat(_, scon) =>
		matchSCon(E, desc, normalise scon, span scon, context, sets)

	   | IDAtPat(_, _, longvid) =>
	       (case StaticEnv.findLongVId(E, longvid)
		  of NONE =>
			succeed(E, desc, context, sets)

		   | SOME(sigma, IdStatus IdStatus.v) =>
			succeed(E, desc, context, sets)

		   | SOME(sigma, IdStatus IdStatus.e) =>
			matchExCon(E, desc, longvid, NONE, context, sets)

		   | SOME((_,tau), IdStatus IdStatus.c) =>
		     let
			val vid  = LongVId.toId longvid
			val span = TyName.span(Type.tyname(Type.range tau))
		     in
			matchCon(E, desc, vid, span, NONE, context, sets)
		     end

		   | SOME(_, TyName t) =>
		     (* [RFC: Views] *)
		     let
			val vid  = LongVId.toId longvid
			val span = TyName.span t
		     in
			matchCon(E, desc, vid, span, NONE, context, sets)
		     end
	       )

	   | RECORDAtPat(_, patrow_opt) =>
		matchPatRowOpt(E, desc, patrow_opt, context, sets)

	   | PARAtPat(_, pat) =>
		matchPat(E, desc, pat, context, sets)


    and matchPat(E, desc, pat, context, sets) =
	case pat
	  of ATPat(_, atpat) =>
		matchAtPat(E, desc, atpat, context, sets)

	   | CONPat(_, _, longvid, atpat) =>
	       (if LongVId.explode longvid = ([], VId.fromString "ref") then
		    matchRef(E, desc, atpat, context, sets)
		else case StaticEnv.findLongVId(E, longvid)
		  of SOME(sigma, IdStatus IdStatus.e) =>
			matchExCon(E, desc, longvid, SOME atpat, context, sets)

		   | SOME((_,tau), IdStatus IdStatus.c) =>
		     let
			val vid  = LongVId.toId longvid
			val span = TyName.span(Type.tyname(Type.range tau))
		     in
			matchCon(E, desc, vid, span, SOME atpat, context, sets)
		     end

		   | SOME(_, TyName t) =>
		     (* [RFC: Views] *)
		     let
			val vid  = LongVId.toId longvid
			val span = TyName.span t
		     in
			matchCon(E, desc, vid, span, SOME atpat, context, sets)
		     end

		   | _ => raise Fail "CheckPattern.matchPat: \
				     \invalid constructed pattern"
	       )

	  | COLONPat(_, pat, ty) =>
		matchPat(E, desc, pat, context, sets)

	  | ASPat(I, pat1, pat2) =>
		matchPat(E, desc, pat1, AS1'(context, I, pat2),
			 extend(sets, #conjs, I))

	  | BARPat(_, pat1, pat2) =>
	    let
		val I1 = infoPat pat1
		val I2 = infoPat pat2
	    in
		matchPat(E, desc, pat1, BAR1'(context, I1, pat2),
			 extend(extend(sets, #alts, I1), #alts, I2))
	    end

	  (* [RFC: Nested matches] *)
	  | WITHPat(I, pat1, pat2, exp) =>
	    if RegionSet.member(#withs sets, I) then
		matchPat(E, desc, pat1, WITH'(context, I, isExhaustive(E, pat2)),
			 sets)
	    else
		matchPat(E, desc, pat1, WITH'(context, I, checkPat'(E, pat2)),
			 extend(sets, #withs, I))


    and matchPatRowOpt(E, desc, patrow_opt, context, sets) =
	let
	    val descs = case desc
			  of ANY          => LabMap.empty
			   | RECORD descs => descs
			   | _ => raise Fail "CheckPattern.matchPatRowOpt: \
					     \invalid record pattern"
	in
	    case patrow_opt
	      of SOME(FIELDPatRow(_, lab, pat, patrow_opt')) =>
		 let
		     val desc' = case LabMap.find(descs, lab)
				   of NONE       => ANY
				    | SOME desc' => desc'
		 in
		     matchPat(E, desc', pat,
			      RECORD'(context, descs, lab, patrow_opt'), sets)
		 end

	       | _ =>
		 succeed(E, RECORD(descs), context, sets)
	end


    and matchSCon(E, desc, scon, span, context, sets) =
	let
	    val descSucc       = SCON scon
	    fun descFail scons = NOT_SCON(SConSet'.add(scons, scon))
	in
	    case desc
	      of ANY =>
		 branch(succeed(E, descSucc, context, sets),
			fail(E, descFail SConSet'.empty, context, sets)
		       )

	       | SCON scon' =>
		 if scon = scon' then
		     succeed(E, desc, context, sets)
		 else
		     fail(E, desc, context, sets)

	       | NOT_SCON scons =>
		 if SConSet'.member(scons, scon) then
		     fail(E, desc, context, sets)
		 else if SConSet'.numItems scons = span - 1 then
		     succeed(E, descSucc, context, sets)
		 else
		     branch(succeed(E, descSucc, context, sets),
			    fail(E, descFail scons, context, sets)
			   )

	       | _ => raise Fail "CheckPattern.matchSCon: type error"
	end


    and matchExCon(E, desc, longvid, atpat_opt, context, sets) =
	let
	    val context' = EXCON'(context, longvid)
	    val descSucc = EXCON(longvid, NONE)
	    fun descFail longvids =
		NOT_EXCON(LongVIdSet.add(longvids, longvid))
	in
	    case desc
	      of ANY =>
		 branch(matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
				    context, context', sets),
			fail(E, descFail LongVIdSet.empty, context, sets)
		       )

	       | EXCON(longvid', desc_opt) =>
		 if longvid = longvid' then
		     matchArgOpt(E, descSucc, desc_opt, atpat_opt,
				 context, context', sets)
		 else
		     fail(E, desc, context, sets)

	       | NOT_EXCON longvids =>
		 if LongVIdSet.member(longvids, longvid) then
		     fail(E, desc, context, sets)
		 else
		     branch(matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
					context, context', sets),
			    fail(E, descFail longvids, context, sets)
			   )

	       | _ => raise Fail "CheckPattern.matchExCon: type error"
	end


    and matchCon(E, desc, vid, span, atpat_opt, context, sets) =
	let
	    val context'      = CON'(context, vid)
	    val descSucc      = CON(vid, NONE)
	    fun descFail vids = NOT_CON(VIdSet.add(vids, vid))
	in
	    case desc
	      of ANY =>
		 if span = 1 then
		     matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
				 context, context', sets)
		 else
		     branch(matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
					context, context', sets),
			    fail(E, descFail VIdSet.empty, context, sets)
			   )

	       | CON(vid', desc_opt) =>
		 if vid = vid' then
		     matchArgOpt(E, descSucc, desc_opt, atpat_opt,
				 context, context', sets)
		 else
		     fail(E, desc, context, sets)

	       | NOT_CON vids =>
		 if VIdSet.member(vids, vid) then
		     fail(E, desc, context, sets)
		 else if VIdSet.numItems vids = span - 1 then
		     matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
				 context, context', sets)
		 else
		     branch(matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
					context, context', sets),
			    fail(E, descFail vids, context, sets)
			   )

	       | _ => raise Fail "CheckPattern.matchCon: type error"
	end


    and matchRef(E, desc, atpat, context, sets) =
	let
	    val desc' = case desc
			  of ANY       => ANY
			   | REF desc' => desc'
			   | _ => raise Fail "CheckPattern.matchRef: \
					     \invalid reference pattern"
	in
	    matchAtPat(E, desc', atpat, REF' context, sets)
	end


    and matchArgOpt(E, desc, desc_opt, atpat_opt, context, context', sets) =
	case atpat_opt
	  of NONE =>
		succeed(E, desc, context, sets)

	   | SOME atpat =>
		matchAtPat(E, valOf desc_opt, atpat, context', sets)


    and succeed(E, desc, EXCON'(context, longvid), sets) =
	    succeed(E, EXCON(longvid, SOME desc), context, sets)

      | succeed(E, desc, CON'(context, vid), sets) =
	    succeed(E, CON(vid, SOME desc), context, sets)

      | succeed(E, desc, REF'(context), sets) =
	    succeed(E, REF(desc), context, sets)

      | succeed(E, desc, RECORD'(context, descs, lab, patrow_opt), sets) =
	    matchPatRowOpt(E, RECORD(LabMap.insert(descs, lab, desc)),
			   patrow_opt, context, sets)

      (* [RFC: Conjunctive patterns] *)
      | succeed(E, desc, AS1'(context, I, pat), sets) =
	    matchPat(E, desc, pat, AS2'(context, I), sets)

      | succeed(E, desc, AS2'(context, I), sets) =
	    succeed(E, desc, context, extend(sets, #reachedConjs, I))

      (* [RFC: Disjunctive patterns] *)
      | succeed(E, desc, BAR1'(context, I, pat), sets) =
	    succeed(E, desc, context, extend(sets, #reachedAlts, I))

      | succeed(E, desc, BAR2'(context, I), sets) =
	    succeed(E, desc, context, extend(sets, #reachedAlts, I))

      (* [RFC: Nested matches] *)
      | succeed(E, desc, WITH'(context, I, true), sets) =
	    succeed(E, invalidateRefs desc, context,
		    extend(sets, #reachedWiths, I))

      (* [RFC: Nested matches] *)
      | succeed(E, desc, WITH'(context, I, false), sets) =
	let
	    val desc' = invalidateRefs desc
	    val sets' = extend(sets, #reachedWiths, I)
	in
	    branch(succeed(E, desc', context, sets'),
		   fail(E, desc', context, sets'))
	end

      | succeed(E, desc, MATCH'(I, match_opt), sets) =
	    skip(match_opt, extend(sets, #reachedMatches, I))


    and skip (SOME(Match(_, mrule, match_opt)), sets) =
	    skip(match_opt, extend(sets, #matches, infoMrule mrule))

      | skip (NONE, sets) =
	    ( sets, true )


    and fail(E, desc, EXCON'(context, longvid), sets) =
	    fail(E, EXCON(longvid, SOME desc), context, sets)

      | fail(E, desc, CON'(context, vid), sets) =
	    fail(E, CON(vid, SOME desc), context, sets)

      | fail(E, desc, REF' context, sets) =
	    fail(E, REF desc, context, sets)

      | fail(E, desc, RECORD'(context, descs, lab, patrow_opt), sets) =
	    fail(E, RECORD(LabMap.insert(descs, lab, desc)), context, sets)

      (* [RFC: Conjunctive patterns] *)
      | fail(E, desc, AS1'(context, I, pat), sets) =
	    fail(E, desc, context, sets)

      | fail(E, desc, AS2'(context, I), sets) =
	    fail(E, desc, context, sets)

      (* [RFC: Disjunctive patterns] *)
      | fail(E, desc, BAR1'(context, I, pat), sets) =
	    matchPat(E, desc, pat, BAR2'(context, infoPat pat), sets)

      | fail(E, desc, BAR2'(context, I), sets) =
	    fail(E, desc, context, sets)

      (* [RFC: Nested matches] *)
      | fail(E, desc, WITH'(context, I, _), sets) =
	    fail(E, desc, context, sets)

      | fail(E, desc, MATCH'(I, SOME match), sets) =
	    matchMatch(E, desc, match, sets)

      | fail(E, desc, MATCH'(I, NONE), sets) =
	    ( sets, false )


    (* Checking matches [Section 4.11, item 2; RFC: Conjunctive patterns;
     *                                         RFC: Disjunctive patterns;
     *                                         RFC: Nested matches] *)

    and checkMatch(E, match) =
	let
	    val (sets, exhaustive) = matchMatch(E, ANY, match, emptySets)
	    val unreachedMatches =
		RegionSet.difference(#matches sets, #reachedMatches sets)
	    val unreachedAlts =
		RegionSet.difference(#alts sets, #reachedAlts sets)
	    val unreachedConjs =
		RegionSet.difference(#conjs sets, #reachedConjs sets)
	in
	    RegionSet.app (fn I => Error.warning(I, "inconsistent pattern"))
	        unreachedConjs;
	    RegionSet.app (fn I => Error.warning(I, "redundant pattern"))
	        unreachedAlts;
	    RegionSet.app (fn I => Error.warning(I, "redundant match rule"))
	        unreachedMatches;
	    if exhaustive then () else
		Error.warning(infoMatch match, "match not exhaustive")
	end



    (* Checking single patterns [Section 4.11, item 3] *)

    and checkPat'(E, pat) =
	let
	    val (sets, exhaustive) =
		matchPat(E, ANY, pat, MATCH'(Source.nowhere, NONE), emptySets)
	    val unreachedWiths =
		RegionSet.difference(#withs sets, #reachedWiths sets)
	    val unreachedAlts =
		RegionSet.difference(#alts sets, #reachedAlts sets)
	    val unreachedConjs =
		RegionSet.difference(#conjs sets, #reachedConjs sets)
	in
	    RegionSet.app (fn I => Error.warning(I, "inconsistent pattern"))
	        unreachedConjs;
	    RegionSet.app (fn I => Error.warning(I, "redundant pattern"))
	        unreachedAlts;
	    RegionSet.app (fn I => Error.warning(I, "redundant pattern"))
	        unreachedWiths;
	    exhaustive
	end


    (* Export *)

    and isExhaustive(E, pat) =
	#2(matchPat(E, ANY, pat, MATCH'(Source.nowhere, NONE), emptySets))

    val viewMatch = fn(E, match) => viewMatch(E, VAR, match)
    val viewPat   = fn(E, pat) => ignore(viewPat(E, VAR, pat))

    fun checkPat(E, pat) =
	if checkPat'(E, pat) then
	    ()
	else
	    Error.warning(infoPat pat, "pattern not exhaustive")
end;
