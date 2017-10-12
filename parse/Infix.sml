(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML infix resolution
 *
 * Definition, Section 2.6
 * + RFC: Record extension
 * + RFC: Nested matches
 *)


structure Infix :> INFIX =
struct
    (* Import *)

    open GrammarCore
    open Error


    (* Type definitions *)

    datatype Assoc = LEFT | RIGHT

    type InfStatus = Assoc * int

    type InfEnv    = InfStatus VIdMap.map		(* [J] *)


    (* Modifying infix environments *)

    val empty = VIdMap.empty

    fun assign(J, vids, infstatus) =
	let
	    fun insert(vid, J) = VIdMap.insert(J, vid, infstatus)
	in
	    List.foldl insert J vids
	end

    fun cancel(J, vids) =
	let
	    fun remove(vid, J) = VIdMap.delete(J, vid)
	in
	    List.foldl remove J vids
	end


    (* Categorisation of atomic expressions and patterns *)

    datatype 'a FixityCategory = NONFIX of 'a
			       | INFIX  of InfStatus * VId * Info

    fun isInfix J (longvid) =
	LongVId.isUnqualified longvid andalso
	VIdMap.find(J, LongVId.toId longvid) <> NONE

    fun categoriseLongVId J (atomic, I, longvid) =
	if LongVId.isUnqualified longvid then
	    let
		val vid = LongVId.toId longvid
	    in
		case VIdMap.find(J, vid)
		  of NONE           => NONFIX(atomic)
		   | SOME infstatus => INFIX(infstatus, vid, I)
	    end
	else
	    NONFIX(atomic)

    fun categoriseAtExp J (atexp as IDAtExp(I, SANSOp, longvid)) =
	    categoriseLongVId J (atexp, I, longvid)
      | categoriseAtExp J (atexp) = NONFIX(atexp)

    fun categoriseAtPat J (atpat as IDAtPat(I, SANSOp, longvid)) =
	    categoriseLongVId J (atpat, I, longvid)
      | categoriseAtPat J (atpat) = NONFIX(atpat)



    (* Resolving infixing [Section 2.6] *)

    fun parse(app, infapp, es) =
	let
	    fun loop(NONFIX(e)::[], []) = e

	      | loop(NONFIX(e2)::NONFIX(e1)::s', i) =
		    (* reduce nonfix application *)
		    loop(NONFIX(app(e1, e2))::s', i)

	      | loop(s, NONFIX(e)::i') =
		    (* shift *)
		    loop(NONFIX(e)::s, i')

	      | loop(s as NONFIX(e)::[], INFIX(x)::i') =
		    (* shift *)
		    loop(INFIX(x)::s, i')

	      | loop(NONFIX(e2)::INFIX(_,vid,_)::NONFIX(e1)::s', []) =
		    (* reduce infix application *)
		    loop(NONFIX(infapp(e1, vid, e2))::s', [])

	      | loop(s as NONFIX(e2)::INFIX((a1,p1),vid1,I1)::NONFIX(e1)::s',
		       i as INFIX(x2 as ((a2,p2),vid2,I2))::i') =
		if p1 > p2 then
		    (* reduce infix application *)
		    loop(NONFIX(infapp(e1, vid1, e2))::s', i)
		else if p1 < p2 then
		    (* shift *)
		    loop(INFIX(x2)::s, i')
		else if a1 <> a2 then
		    error(Source.over(I1,I2), "conflicting infix associativity")
		else if a1 = LEFT then
		    (* reduce infix application *)
		    loop(NONFIX(infapp(e1, vid1, e2))::s', i)
		else (* a1 = RIGHT *)
		    (* shift *)
		    loop(INFIX(x2)::s, i')

	      | loop(INFIX(_, vid, I)::s, []) =
		    errorVId(I, "misplaced infix identifier ", vid)

	      | loop(INFIX(x)::s, INFIX(_, vid, I)::i) =
		    errorVId(I, "misplaced infix identifier ", vid)

	      | loop([], INFIX(_, vid, I)::i) =
		    errorVId(I, "misplaced infix identifier ", vid)

	      | loop _ = raise Fail "Infix.parse: inconsistency"
	in
	    loop([], es)
	end


    (* Resolving infixed expressions [Section 2.6] *)

    fun atExp atexp = ATExp(infoAtExp atexp, atexp)

    fun appExp(atexp1, atexp2) =
	let
	    val I1 = infoAtExp atexp1
	    val I2 = infoAtExp atexp2
	    val I  = Source.over(I1, I2)
	in
	    PARAtExp(I, APPExp(I, atExp atexp1, atexp2))
	end

    fun pairExp(atexp1, atexp2) =
	let
	    val I1	= infoAtExp atexp1
	    val I2	= infoAtExp atexp2
	    val lab1	= Lab.fromInt 1
	    val lab2	= Lab.fromInt 2
	    val exprow2	= FIELDExpRow(I2, lab2, atExp atexp2, NONE)
	    val exprow1	= FIELDExpRow(I1, lab1, atExp atexp1, SOME exprow2)
	in
	    RECORDAtExp(Source.over(I1,I2), SOME exprow1)
	end

    fun infExp(atexp1, vid, atexp2) =
	let
	    val Ivid	= Source.between(infoAtExp atexp1, infoAtExp atexp2)
	    val longvid	= LongVId.fromId vid
	    val atexp1'	= IDAtExp(Ivid, SANSOp, longvid)
	    val atexp2'	= pairExp(atexp1, atexp2)
	in
	    appExp(atexp1', atexp2')
	end


    fun parseExp(J, atexps) =
	let
	    val atexp = parse(appExp, infExp,
			      List.map (categoriseAtExp J) atexps)
	in
	    atExp atexp
	end


    (* Resolving infixed patterns [Section 2.6] *)

    fun atPat atpat = ATPat(infoAtPat atpat, atpat)

    fun appPat(IDAtPat(I1, op_opt, longvid), atpat) =
	let
	    val I2 = infoAtPat atpat
	    val I  = Source.over(I1, I2)
	in
	    PARAtPat(I, CONPat(I, op_opt, longvid, atpat))
	end

      (* [RFC: Nested matches] *)
      | appPat(PARAtPat(I, WITHPat(I2, pat1, _, exp)), atpat) =
        (* The restricted productions in the parser guarantee that the
         * argument must stem from rewriting an atomic ?atexp pattern.
         *)
	let
	    val I3      = infoAtPat atpat
	    val longvid = LongVId.fromId(VId.fromString "SOME")
	    val pat2    = CONPat(I3, SANSOp, longvid, atpat)
	in
	    PARAtPat(I, WITHPat(I2, pat1, pat2, exp))
	end

      | appPat(_, atpat) =
	    error(infoAtPat atpat, "misplaced atomic pattern")

    fun pairPat(atpat1, atpat2) =
	let
	    val I1	= infoAtPat atpat1
	    val I2	= infoAtPat atpat2
	    val lab1	= Lab.fromInt 1
	    val lab2	= Lab.fromInt 2
	    val patrow2	= FIELDPatRow(I2, lab2, atPat atpat2, NONE)
	    val patrow1	= FIELDPatRow(I1, lab1, atPat atpat1, SOME patrow2)
	in
	    RECORDAtPat(Source.over(I1,I2), SOME patrow1)
	end

    fun infPat(atpat1, vid, atpat2) =
	let
	    val Ivid	= Source.between(infoAtPat atpat1, infoAtPat atpat2)
	    val longvid	= LongVId.fromId vid
	    val atpat1'	= IDAtPat(Ivid, SANSOp, longvid)
	    val atpat2'	= pairPat(atpat1, atpat2)
	in
	    appPat(atpat1', atpat2')
	end


    fun parsePat(J, atpats) =
	let
	    val atpat = parse(appPat, infPat,
			      List.map (categoriseAtPat J) atpats)
	in
	    atPat atpat
	end


    (* Resolving fun match rules [Figure 21, note] *)

    fun parseFmrule(J, atpats) =
	(*
	 * Allowed is the following:
	 * (1) <op> vid atpat+
	 * (2) (atpat infix_vid atpat) atpat*
	 * (3) atpat infix_vid atpat
	 *)
	let
	    fun checkNonfixity []           = true
	      | checkNonfixity(NONFIX _::t) = checkNonfixity t
	      | checkNonfixity(INFIX(_, vid, I)::t) =
		    errorVId(I, "misplaced infix identifier ", vid)

	    fun maybeNonfixClause(ps) =
		case List.hd atpats
		  of IDAtPat(I, op_opt, longvid) =>
			if not(LongVId.isUnqualified longvid) then
			    errorLongVId(I, "misplaced long identifier ",
					 longvid)
			else if List.length atpats < 2 then
			    error(I, "missing function arguments")
			else
			    ( checkNonfixity ps	(* including 1st *)
			    ; ( op_opt, LongVId.toId longvid, List.tl atpats )
			    )
		   | WILDCARDAtPat(I) =>
			error(I, "misplaced wildcard pattern")
		   | SCONAtPat(I, _) =>
			error(I, "misplaced constant pattern")
		   | RECORDAtPat(I, _) =>
			error(I, "misplaced record or tuple pattern")
		   | PARAtPat(I, _) =>
			error(I, "misplaced parenthesised pattern")

	    fun maybeParenthesisedInfixClause(ps) =
	        case List.hd ps
		  of NONFIX(PARAtPat(_, CONPat(I, SANSOp, longvid, atpat))) =>
			if not(LongVId.isUnqualified longvid) then
			    errorLongVId(I, "misplaced long identifier ",
					 longvid)
			else if not(isInfix J longvid) then
			    error(I, "misplaced non-infix pattern")
			else
			    (* Now, longvid has infix status but is sans `op',
			       so it can only result from resolving an
			       appropriate infix construction. *)
			    ( checkNonfixity(List.tl ps)
			    ; ( SANSOp, LongVId.toId longvid,
				atpat::List.tl atpats )
			    )

	           | NONFIX(PARAtPat(_, ATPat(_, atpat as PARAtPat _))) =>
			maybeParenthesisedInfixClause(NONFIX(atpat)::List.tl ps)

	           | NONFIX(PARAtPat(_, pat)) =>
			error(infoPat pat, "misplaced non-infix pattern")

	 	   | _ => maybeNonfixClause(ps)

	    fun maybePlainInfixClause(ps) =
	        case ps
	          of [NONFIX atpat1, INFIX(_, vid, I), NONFIX atpat2] =>
			 ( SANSOp, vid, pairPat(atpat1, atpat2)::[] )

		   | _ => maybeParenthesisedInfixClause(ps)
	in
	    maybePlainInfixClause(List.map (categoriseAtPat J) atpats)
	end
end;
