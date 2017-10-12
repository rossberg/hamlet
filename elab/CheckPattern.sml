(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML consistency of patterns and matches
 *
 * Definition, Section 4.11
 *
 * Notes: see CHECK_PATTERN-sig.sml
 *)

structure CheckPattern : CHECK_PATTERN =
struct
  (* Import *)

  open SyntaxCore
  open AnnotationCore


  (*
   * Algorithm loosely based on:
   *    Peter Sestoft. "ML pattern matching compilation and partial evaluation",
   *    in: Dagstuhl Seminar on Partial Evaluation,
   *        Lecture Notes in Computer Science, Springer-Verlag 1996
   *)

  (* Value description *)

  structure SConSet' =
      FinSetFn(type ord_key = string; val compare = String.compare)
  structure LongVIdSet =
      FinSetFn(type ord_key = LongVId.longId; val compare = LongVId.compare)

  type SCon'      = string
  type SConSet'   = SConSet'.set
  type VIdSet     = VIdSet.set
  type LongVIdSet = LongVIdSet.set
  type 'a LabMap  = 'a LabMap.map

  datatype description =
      ANY
    | SCON      of SCon'
    | NOT_SCON  of SConSet'
    | EXCON     of LongVId.longId * description option
    | NOT_EXCON of LongVIdSet
    | CON       of VId.Id * description option
    | NOT_CON   of VIdSet
    | RECORD    of description LabMap

  datatype context =
      EXCON'  of context * LongVId.longId
    | CON'    of context * VId.Id
    | RECORD' of context * description LabMap * Lab.Lab * PatRow option
    | MATCH'  of Source.loc * Match option


  (* Normalise special constants *)

  local
    open LibrarySVal
  in
    fun normalise(SCon.INT(b, sc)@@A) =
          intToString(intFromString(b, sc, try(elab A)))
      | normalise(SCon.WORD(b, sc)@@A) =
          wordToString(wordFromString(b, sc, try(elab A)))
      | normalise(SCon.CHAR(sc)@@A) =
          charToString(charFromString(sc, try(elab A)))
      | normalise(SCon.STRING(sc)@@A) =
          stringToString(stringFromString(sc, try(elab A)))
      | normalise(SCon.REAL(sc)@@A) =
          realToString(realFromString(sc, try(elab A)))
  end

  fun span(scon@@A) =
      case try(elab A) of
        NONE   => 0
      | SOME t => TyName.span t


  (* Result type for static matching *)

  structure LocationSet =
      FinSetFn(type ord_key = Source.loc; val compare = Source.compare)

  type sets   = {matches : LocationSet.set, reached : LocationSet.set}
  type result = sets * bool

  val emptySets = {matches = LocationSet.empty, reached = LocationSet.empty}

  fun update({matches, reached}, lab, f) =
      let
        val sets = {matches = ref matches, reached = ref reached}
      in
        lab sets := f(!(lab sets));
        {matches = !(#matches sets), reached = !(#reached sets)}
      end

  fun extend(sets, lab, region) =
        update(sets, lab, fn L => LocationSet.add(L, region))

  fun branch((sets1, exhaustive1) : result, (sets2, exhaustive2) : result) =
      ( { matches = LocationSet.union(#matches sets1, #matches sets2),
          reached = LocationSet.union(#reached sets1, #reached sets2)
        },
        exhaustive1 andalso exhaustive2
      )


  (* Static pattern matching *)

  fun matchMatch(E, desc, Match(mrule, match_opt)@@_, sets) =
        matchMrule(E, desc, mrule, match_opt, sets)

  and matchMrule(E, desc, Mrule(pat, exp)@@A, match_opt, sets) =
        matchPat(E, desc, pat, MATCH'(loc A, match_opt),
          extend(sets, #matches, loc A))

  and matchAtPat(E, desc, atpat@@_, context, sets) =
      case atpat of
          WILDCARDAtPat =>
            succeed(E, desc, context, sets)
        | SCONAtPat(scon) =>
            matchSCon(E, desc, normalise scon, span scon, context, sets)
        | IDAtPat(_, longvid@@_) =>
            (case StaticEnv.findLongVId(E, longvid) of
                NONE =>
                  succeed(E, desc, context, sets)
              | SOME(sigma, IdStatus.v) =>
                  succeed(E, desc, context, sets)
              | SOME(sigma, IdStatus.e) =>
                  matchExCon(E, desc, longvid, NONE, context, sets)
              | SOME((_, tau), IdStatus.c) =>
                  matchCon(E, desc, LongVId.toId longvid,
                    TyName.span(Type.tyname tau), NONE, context, sets)
            )
        | RECORDAtPat(patrow_opt) =>
            matchPatRowOpt(E, desc, patrow_opt, context, sets)
        | PARAtPat(pat) =>
            matchPat(E, desc, pat, context, sets)

  and matchPat(E, desc, pat@@_, context, sets) =
      case pat of
          ATPat(atpat) =>
            matchAtPat(E, desc, atpat, context, sets)
        | CONPat(_, longvid@@_, atpat) =>
            (case StaticEnv.findLongVId(E, longvid) of
                SOME(sigma, IdStatus.e) =>
                  matchExCon(E, desc, longvid, SOME atpat, context, sets)
              | SOME((_, tau), IdStatus.c) =>
                  matchCon(E, desc, LongVId.toId longvid,
                    TyName.span(Type.tyname tau), SOME atpat, context, sets)
              | _ => raise Fail "CheckPattern.matchPat: invalid CONPat"
            )
        | COLONPat(pat, ty) =>
            matchPat(E, desc, pat, context, sets)
        | ASPat(_, _, _, pat) =>
            matchPat(E, desc, pat, context, sets)

  and matchPatRowOpt(E, desc, patrow_opt, context, sets) =
      let
        val descs =
            case desc of
                ANY          => LabMap.empty
              | RECORD descs => descs
              | _ => raise Fail "CheckPattern.matchPatRowOpt: type error"
      in
        case patrow_opt of
            SOME(FIELDPatRow(lab@@_, pat, patrow_opt')@@_) =>
            let
              val desc' =
                  case LabMap.find(descs, lab) of
                      NONE       => ANY
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
        case desc of
            ANY =>
              branch(
                succeed(E, descSucc, context, sets),
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
                branch(
                  succeed(E, descSucc, context, sets),
                  fail(E, descFail scons, context, sets)
                )
          | _ => raise Fail "CheckPattern.matchSCon: type error"
      end

  and matchExCon(E, desc, longvid, atpat_opt, context, sets) =
      let
        val context' = EXCON'(context, longvid)
        val descSucc = EXCON(longvid, NONE)
        fun descFail longvids = NOT_EXCON(LongVIdSet.add(longvids, longvid))
      in
        case desc of
            ANY =>
              branch(
                matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
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
                branch(
                  matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
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
        case desc of
            ANY =>
              if span = 1 then
                matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
                  context, context', sets)
              else
                branch(
                  matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
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
                branch(
                  matchArgOpt(E, descSucc, SOME ANY, atpat_opt,
                    context, context', sets),
                  fail(E, descFail vids, context, sets)
                )
          | _ => raise Fail "CheckPattern.matchCon: type error"
      end

  and matchArgOpt(E, desc, desc_opt, atpat_opt, context, context', sets) =
        case atpat_opt of
            NONE =>
              succeed(E, desc, context, sets)
          | SOME atpat =>
              matchAtPat(E, valOf desc_opt, atpat, context', sets)

  and succeed(E, desc, EXCON'(context, longvid), sets) =
        succeed(E, EXCON(longvid, SOME desc), context, sets)
    | succeed(E, desc, CON'(context, vid), sets) =
        succeed(E, CON(vid, SOME desc), context, sets)
    | succeed(E, desc, RECORD'(context, descs, lab, patrow_opt), sets) =
        matchPatRowOpt(E, RECORD(LabMap.insert(descs, lab, desc)),
          patrow_opt, context, sets)
    | succeed(E, desc, MATCH'(loc, match_opt), sets) =
        skip(match_opt, extend(sets, #reached, loc))

  and skip (SOME(Match(_@@A, match_opt)@@_), sets) =
        skip(match_opt, extend(sets, #matches, loc A))
    | skip (NONE, sets) =
        (sets, true)

  and fail(E, desc, EXCON'(context, longvid), sets) =
        fail(E, EXCON(longvid, SOME desc), context, sets)
    | fail(E, desc, CON'(context, vid), sets) =
        fail(E, CON(vid, SOME desc), context, sets)
    | fail(E, desc, RECORD'(context, descs, lab, patrow_opt), sets) =
        fail(E, RECORD(LabMap.insert(descs, lab, desc)), context, sets)
    | fail(E, desc, MATCH'(loc, SOME match), sets) =
        matchMatch(E, desc, match, sets)
    | fail(E, desc, MATCH'(loc, NONE), sets) =
        (sets, false)


  (* Checking matches [Section 4.11, item 2] *)

  open StaticObjectsCore

  fun toExhaustive true  = Exhaustive
    | toExhaustive false = NonExhaustive

  fun checkMatch(E, match) =
      let
        val (sets, exhaustive) = matchMatch(E, ANY, match, emptySets)
        val unreached = LocationSet.difference(#matches sets, #reached sets)
      in
        LocationSet.app
          (fn loc => Error.warning(loc, "redundant match rule"))
          unreached;
        if exhaustive then () else
          Error.warning(loc(annotation match), "match not exhaustive");
        toExhaustive exhaustive
      end


  (* Checking single patterns [Section 4.11, item 3] *)

  fun checkPat(E, pat) =
      let
        val (_, exhaustive) =
            matchPat(E, ANY, pat, MATCH'(Source.nowhere, NONE), emptySets)
      in
        if exhaustive then () else
          Error.warning(loc(annotation pat), "pattern not exhaustive");
        toExhaustive exhaustive
      end
end;
