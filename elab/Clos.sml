(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML closure of value environments
 *
 * Definition, Section 4.7 and 4.8
 *)

structure Clos : CLOS =
struct
  (* Import *)

  open SyntaxCore
  open AnnotationCore
  open StaticObjectsCore


  (* Check whether a pattern binds an identifier *)

  fun ?binds(NONE, vid)   = false
    | ?binds(SOME x, vid) = binds(x, vid)

  fun bindsAtPat(WILDCARDAtPat@@_, vid) =
        false
    | bindsAtPat(SCONAtPat(scon)@@_, vid) =
        false
    | bindsAtPat(IDAtPat(_, longvid@@_)@@_, vid) =
      let
        val (strids, vid') = LongVId.explode longvid
      in
        List.null strids andalso vid = vid'
      end
    | bindsAtPat(RECORDAtPat(patrow_opt)@@_, vid) =
        ?bindsPatRow(patrow_opt, vid)
    | bindsAtPat(PARAtPat(pat)@@_, vid) =
        bindsPat(pat, vid)

  and bindsPatRow(DOTSPatRow@@_, vid) =
        false
    | bindsPatRow(FIELDPatRow(lab, pat, patrow_opt)@@_, vid) =
        bindsPat(pat, vid) orelse ?bindsPatRow(patrow_opt, vid)

  and bindsPat(ATPat(atpat)@@_, vid) =
        bindsAtPat(atpat, vid)
    | bindsPat(CONPat(_, longvid, atpat)@@_, vid) =
        bindsAtPat(atpat, vid)
    | bindsPat(COLONPat(pat, ty)@@_, vid) =
        bindsPat(pat, vid)
    | bindsPat(ASPat(_, vid'@@_, ty_opt, pat)@@_, vid) =
        vid = vid' orelse bindsPat(pat, vid)


  (* Non-expansive expressions [Section 4.7] *)

  fun ?isNonExpansive C NONE     = true
    | ?isNonExpansive C (SOME x) = isNonExpansive C x

  fun isNonExpansiveAtExp C (SCONAtExp(scon)@@_) =
        true
    | isNonExpansiveAtExp C (IDAtExp(_, longvid)@@_) =
        true
    | isNonExpansiveAtExp C (RECORDAtExp(exprow_opt)@@_) =
        ?isNonExpansiveExpRow C exprow_opt
    | isNonExpansiveAtExp C (PARAtExp(exp)@@_) =
        isNonExpansiveExp C exp
    | isNonExpansiveAtExp C _ =
        false

  and isNonExpansiveExpRow C (ExpRow(lab, exp, exprow_opt)@@_) =
        isNonExpansiveExp C exp andalso ?isNonExpansiveExpRow C exprow_opt

  and isNonExpansiveExp C (ATExp(atexp)@@_) =
        isNonExpansiveAtExp C atexp
    | isNonExpansiveExp C (APPExp(exp, atexp)@@_) =
        isConExp C exp andalso isNonExpansiveAtExp C atexp
    | isNonExpansiveExp C (COLONExp(exp, ty)@@_) =
        isNonExpansiveExp C exp
    | isNonExpansiveExp C (FNExp(match)@@_) =
        true
    | isNonExpansiveExp C _ =
        false

  and isConAtExp C (PARAtExp(exp)@@_) =
        isConExp C exp
    | isConAtExp C (IDAtExp(_, longvid@@_)@@_) =
        LongVId.explode longvid <> ([], VId.fromString "ref") andalso
        (case Context.findLongVId(C, longvid) of
          SOME(_, is) => is = IdStatus.c orelse is = IdStatus.e
        | NONE        => false
        )
    | isConAtExp C _ =
        false

  and isConExp C (ATExp(atexp)@@_) =
        isConAtExp C atexp
    | isConExp C (COLONExp(ATExp(atexp)@@_, ty)@@_) =
        isConAtExp C atexp
    | isConExp C _ =
        false


  (* Closure [Section 4.8] *)

  fun hasNonExpansiveRHS C (vid, PLAINValBind(pat, exp, valbind_opt)@@_) =
        if bindsPat(pat, vid) then
          isNonExpansiveExp C exp
        else
          hasNonExpansiveRHS C (vid, valOf valbind_opt)
    | hasNonExpansiveRHS C (vid, RECValBind(_)@@_) =
        (* A rec valbind can only contain functions. *)
        true

  fun Clos (C, valbind) VE =
      let
        val tyvarsC = Context.tyvars C
        val undetsC = Context.undetermined C

        fun ClosType vid tau =
            if hasNonExpansiveRHS C (vid, valbind) then
              let
                val tyvars = TyVarSet.difference(Type.tyvars tau, tyvarsC)
                val undets = StampMap.difference(Type.undetermined tau, undetsC)
                val tyvars' = StampMap.map TyVar.invent undets
                val det    = StampMap.map Type.fromTyVar tyvars'
              in
                Type.determine det tau;
                (TyVarSet.listItems tyvars @ StampMap.listItems tyvars', tau)
              end
            else
              ([], tau)
      in
        VIdMap.mapi (fn(vid, ((_, tau), is)) => (ClosType vid tau, is)) VE
      end
end;
