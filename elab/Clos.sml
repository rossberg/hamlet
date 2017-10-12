(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML closure of value environments
 *
 * Definition, Section 4.7 and 4.8
 * + RFC: Record extension
 * + RFC: Conjunctive patterns
 * + RFC: Disjunctive patterns
 * + RFC: Nested matches
 * + RFC: Views
 * + RFC: Monomorphic non-exhaustive bindings
 * + RFC: Simplified recursive value bindings
 * + RFC: First-class modules
 *)

structure Clos : CLOS =
struct
    (* Import *)

    open GrammarCore
    open StaticObjectsCore


    (* Check whether a pattern binds an identifier *)

    fun ?? bindsX(NONE,   vid) = false
      | ?? bindsX(SOME x, vid) = bindsX(x, vid)

    fun bindsAtPat(WILDCARDAtPat(_), vid)   = false
      | bindsAtPat(SCONAtPat(_, scon), vid) = false
      | bindsAtPat(IDAtPat(_, _, longvid), vid) =
	let
	    val (strids,vid') = LongVId.explode longvid
	in
	    List.null strids andalso vid = vid'
	end
      | bindsAtPat(RECORDAtPat(_, patrow_opt), vid) =
	    ??bindsPatRow(patrow_opt, vid)
      | bindsAtPat(PARAtPat(_, pat), vid) = bindsPat(pat, vid)

    (* [RFC: Record extension] *)
    and bindsPatRow(DOTSPatRow(_, pat), vid) = bindsPat(pat, vid)
      | bindsPatRow(FIELDPatRow(_, lab, pat, patrow_opt), vid) =
	    bindsPat(pat, vid) orelse ??bindsPatRow(patrow_opt, vid)

    and bindsPat(ATPat(_, atpat), vid)              = bindsAtPat(atpat, vid)
      | bindsPat(CONPat(_, _, longvid, atpat), vid) = bindsAtPat(atpat, vid)
      | bindsPat(COLONPat(_, pat, ty), vid)         = bindsPat(pat, vid)
      | bindsPat(ASPat(_, pat1, pat2), vid) =
	    bindsPat(pat1, vid) orelse bindsPat(pat2, vid)
      | bindsPat(BARPat(_, pat1, pat2), vid) =
	    bindsPat(pat1, vid) andalso bindsPat(pat2, vid)
      (* [RFC: Nested matches] *)
      | bindsPat(WITHPat(_, pat1, pat2, exp), vid) =
	    bindsPat(pat1, vid) orelse bindsPat(pat2, vid)


    (* Non-expansive expressions [Section 4.7] *)

    fun ?? isNonExpansiveX C  NONE    = true
      | ?? isNonExpansiveX C (SOME x) = isNonExpansiveX C x

    fun isNonExpansiveAtExp C (SCONAtExp(_, scon))         = true
      | isNonExpansiveAtExp C (IDAtExp(_, _, longvid))     = true
      | isNonExpansiveAtExp C (RECORDAtExp(_, exprow_opt)) =
	    ??isNonExpansiveExpRow C exprow_opt
      | isNonExpansiveAtExp C (PARAtExp(_, exp)) = isNonExpansiveExp C exp
      | isNonExpansiveAtExp C  _                 = false

    (* [RFC: Record extension] *)
    and isNonExpansiveExpRow C (DOTSExpRow(_, exp)) = isNonExpansiveExp C exp
      | isNonExpansiveExpRow C (FIELDExpRow(_, lab, exp, exprow_opt)) =
	    isNonExpansiveExp C exp andalso ??isNonExpansiveExpRow C exprow_opt

    and isNonExpansiveExp C (ATExp(_, atexp)) =
	    isNonExpansiveAtExp C atexp
      | isNonExpansiveExp C (APPExp(_, exp, atexp)) =
	    isConExp C exp andalso isNonExpansiveAtExp C atexp
      | isNonExpansiveExp C (COLONExp(_, exp, ty)) = isNonExpansiveExp C exp
      | isNonExpansiveExp C (FNExp(_, match))      = true
      | isNonExpansiveExp C  _                     = false

    and isConAtExp C (PARAtExp(_, exp))       = isConExp C exp
      | isConAtExp C (IDAtExp(_, _, longvid)) =
	    LongVId.explode longvid <> ([],VId.fromString "ref") andalso
	    (* [RFC: Views] *)
	    (case Context.findLongVId(C, longvid)
	       of SOME(_,vs) => vs <> IdStatus IdStatus.v
		| NONE       => false
	    )
      | isConAtExp C  _ = false

    and isConExp C (ATExp(_, atexp))                  = isConAtExp C atexp
      | isConExp C (COLONExp(_, ATExp(_, atexp), ty)) = isConAtExp C atexp
      | isConExp C  _                                 = false

    (* Non-expansive patterns [Section 4.7; RFC: Nested matches] *)

    fun ?? isNonExpansiveX NONE    = true
      | ?? isNonExpansiveX(SOME x) = isNonExpansiveX x

    fun isNonExpansiveAtPat(WILDCARDAtPat(_))           = true
      | isNonExpansiveAtPat(SCONAtPat(_, scon))         = true
      | isNonExpansiveAtPat(IDAtPat(_, _, longvid))     = true
      | isNonExpansiveAtPat(RECORDAtPat(_, patrow_opt)) =
	    ??isNonExpansivePatRow patrow_opt
      | isNonExpansiveAtPat(PARAtPat(_, pat))           = isNonExpansivePat pat

    (* [RFC: Record extension] *)
    and isNonExpansivePatRow(DOTSPatRow(_, pat)) = isNonExpansivePat pat
      | isNonExpansivePatRow(FIELDPatRow(_, lab, pat, patrow_opt)) =
	    isNonExpansivePat pat andalso ??isNonExpansivePatRow patrow_opt

    and isNonExpansivePat(ATPat(_, atpat)) =
	    isNonExpansiveAtPat atpat
      | isNonExpansivePat(CONPat(_, _, longvid, atpat)) =
	    isNonExpansiveAtPat atpat
      | isNonExpansivePat(COLONPat(_, pat, ty)) =
	    isNonExpansivePat pat
      | isNonExpansivePat(ASPat(_, pat1, pat2)) =
	    isNonExpansivePat pat1 andalso isNonExpansivePat pat2
      | isNonExpansivePat(BARPat(_, pat1, pat2)) =
	    isNonExpansivePat pat1 andalso isNonExpansivePat pat2
      | isNonExpansivePat(WITHPat(_, pat1, pat2, exp)) = false


    (* Closure [Section 4.8; RFC: Monomorphic non-exhaustive bindings
     *                       RFC: Simplified recursive value bindings] *)

    fun isExhaustiveAndNonExpansive C
	    (vid, ValBind(I, pat, exp, valbind_opt)) =
	if bindsPat(pat, vid) then
	    (* [RFC: Monomorphic non-exhaustive bindings] *)
	    isNonExpansivePat pat andalso isNonExpansiveExp C exp andalso
	    CheckPattern.isExhaustive(Context.Eof C, pat)
	else
	    isExhaustiveAndNonExpansive C (vid, valOf valbind_opt)


    fun Clos (C,valbind) VE =
	let
	    val tyvarsC = Context.tyvars C
	    val undetsC = Context.undetermined C

	    fun ClosType vid tau =
		if isExhaustiveAndNonExpansive C (vid, valbind) then
		    let
			val tyvars =
			    TyVarSet.difference(Type.tyvars tau, tyvarsC)
			val undets =
			    StampMap.difference(Type.undetermined tau, undetsC)
			val tyvars' = StampMap.map TyVar.invent undets
			val det     = StampMap.map Type.fromTyVar tyvars'
		    in
			(TyVarSet.listItems tyvars @ StampMap.listItems tyvars',
			 Type.determine det tau)
		    end
		else
		    ( [], tau )
	in
	    VIdMap.mapi (fn(vid, ((_,tau),vs)) => (ClosType vid tau, vs)) VE
	end
end;
