(*
 * (c) Andreas Rossberg 2013
 *
 * Free and bound identifiers in the core language.
 *)

structure IdSetCore : ID_SET_CORE =
struct
  (* Import *)

  open SyntaxCore
  open AnnotationCore


  (* Identifier sets *)

  type IdSet = {vids : VIdSet.set, strids : StrIdSet.set}

  val empty = {vids = VIdSet.empty, strids = StrIdSet.empty}
  fun fromVIds vids = {vids = VIdSet.fromList vids, strids = StrIdSet.empty}
  fun fromStrIds strids =
        {vids = VIdSet.empty, strids = StrIdSet.fromList strids}

  fun {vids = vids1, strids = strids1} + {vids = vids2, strids = strids2} =
        { vids   = VIdSet.union(vids1, vids2),
          strids = StrIdSet.union(strids1, strids2) }
  fun {vids = vids1, strids = strids1} - {vids = vids2, strids = strids2} =
        { vids   = VIdSet.difference(vids1, vids2),
          strids = StrIdSet.difference(strids1, strids2) }

  fun disjoint(
        {vids = vids1, strids = strids1}, {vids = vids2, strids = strids2}
      ) =
        VIdSet.disjoint(vids1, vids2) andalso
        StrIdSet.disjoint(strids1, strids2)

  fun boundValEnv VE = fromVIds(VIdMap.listKeys VE)
  fun boundStrEnv SE = fromStrIds(StrIdMap.listKeys SE)
  fun boundEnv(StaticObjectsCore.Env(SE, TE, VE)) =
        boundValEnv VE + boundStrEnv SE


  (* Free identifiers *)

  fun ?free NONE    = empty
    | ?free(SOME x) = free x

  fun freeVId(vid@@_)     = fromVIds[vid]
  fun freeStrId(strid@@_) = fromStrIds[strid]

  fun freeLongId (explode, fromIds) (longid@@_) =
      case explode longid of
        ([], id) => fromIds[id]
      | (strid::strids, id) => fromStrIds[strid]

  fun freeLongVId longid     = freeLongId (LongVId.explode, fromVIds) longid
  fun freeLongStrId longid   = freeLongId (LongStrId.explode, fromStrIds) longid
  fun freeLongStrIds longids =
        List.foldr op+ empty (List.map freeLongStrId longids)

  fun freeAtExp(SCONAtExp(scon)@@_) =
        empty
    | freeAtExp(IDAtExp(_, longvid)@@_) =
        freeLongVId longvid
    | freeAtExp(RECORDAtExp(exprow_opt)@@_) =
        ?freeExpRow exprow_opt
    | freeAtExp(LETAtExp(dec as _@@A1, exp)@@_) =
        freeDec dec + (freeExp exp - boundEnv(get(elab A1)))
    | freeAtExp(PARAtExp(exp)@@_) =
        freeExp exp

  and freeExpRow(ExpRow(lab, exp, exprow_opt)@@_) =
        freeExp exp + ?freeExpRow exprow_opt

  and freeExp(ATExp(atexp)@@_) =
        freeAtExp atexp
    | freeExp(APPExp(exp, atexp)@@_) =
        freeExp exp + freeAtExp atexp
    | freeExp(COLONExp(exp, ty)@@_) =
        freeExp exp
    | freeExp(HANDLEExp(exp, match)@@_) =
        freeExp exp + freeMatch match
    | freeExp(RAISEExp(exp)@@_) =
        freeExp exp
    | freeExp(FNExp(match)@@_) =
        freeMatch match

  and freeMatch(Match(mrule, match_opt)@@_) =
        freeMrule mrule + ?freeMatch match_opt

  and freeMrule(Mrule(pat as _@@A1, exp)@@_) =
        freePat pat + (freeExp exp - boundValEnv(#1(get(elab A1))))


  and freeDec(VALDec(tyvarseq, valbind)@@A) =
        freeValBind valbind
    | freeDec(TYPEDec(typbind)@@_) =
        empty
    | freeDec(DATATYPEDec(datbind)@@_) =
        empty
    | freeDec(DATATYPE2Dec(tycon, longtycon@@_)@@_) =
        empty
    | freeDec(ABSTYPEDec(datbind as _@@A1, dec)@@_) =
        freeDec dec - boundValEnv(#1(get(elab A1)))
    | freeDec(EXCEPTIONDec(exbind)@@_) =
        freeExBind exbind
    | freeDec(LOCALDec(dec1 as _@@A1, dec2)@@_) =
        freeDec dec1 + (freeDec dec2 - boundEnv(get(elab A1)))
    | freeDec(OPENDec(longstrids)@@_) =
        List.foldr
          (fn(longstrid, free) => freeLongStrId longstrid + free)
          empty longstrids
    | freeDec(EMPTYDec@@_) =
        empty
    | freeDec(SEQDec(dec1 as _@@A1, dec2)@@_) =
        freeDec dec1 + (freeDec dec2 - boundEnv(get(elab A1)))

  and freeValBind(PLAINValBind(pat, exp, valbind_opt)@@_) =
        freePat pat + freeExp exp + ?freeValBind valbind_opt
    | freeValBind(RECValBind(valbind)@@A) =
        freeValBind valbind - boundValEnv(get(elab A))

  and freeExBind(NEWExBind(_, vid, ty_opt, exbind_opt)@@_) =
        ?freeExBind exbind_opt
    | freeExBind(EQUALExBind(_, vid, _, longvid, exbind_opt)@@_) =
        freeLongVId longvid + ?freeExBind exbind_opt


  and freeAtPat(WILDCARDAtPat@@_) =
        empty
    | freeAtPat(SCONAtPat(scon)@@_) =
        empty
    | freeAtPat(IDAtPat(_, longvid as _@@A)@@_) =
        if #2(get(elab A)) = IdStatus.v then empty else freeLongVId longvid
    | freeAtPat(RECORDAtPat(patrow_opt)@@_) =
        ?freePatRow patrow_opt
    | freeAtPat(PARAtPat(pat)@@_) =
        freePat pat

  and freePatRow(DOTSPatRow@@_) =
        empty
    | freePatRow(FIELDPatRow(lab, pat, patrow_opt)@@_) =
        freePat pat + ?freePatRow patrow_opt

  and freePat(ATPat(atpat)@@_) =
        freeAtPat atpat
    | freePat(CONPat(_, longvid, atpat)@@_) =
        freeLongVId longvid + freeAtPat atpat
    | freePat(COLONPat(pat, ty)@@_) =
        freePat pat
    | freePat(ASPat(_, vid, ty_opt, pat)@@_) =
        freePat pat


  (* Bound identifiers *)

  fun ?bound NONE    = []
    | ?bound(SOME x) = bound x

  fun boundValBind(PLAINValBind(pat@@A, exp, valbind_opt)@@_) =
        VIdSet.fromList(VIdMap.listKeys(#1(get(elab A)))) ::
          ?boundValBind valbind_opt
    | boundValBind(RECValBind(valbind)@@_) =
        boundValBind valbind

  fun boundExBind(NEWExBind(_, vid@@_, _, exbind_opt)@@_) =
        vid :: ?boundExBind exbind_opt
    | boundExBind(EQUALExBind(_, vid@@_, _, _, exbind_opt)@@_) =
        vid :: ?boundExBind exbind_opt
end;
