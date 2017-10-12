(*
 * (c) Andreas Rossberg 2013
 *
 * Free and bound identifiers in the module language.
 *)

structure IdSetModule : ID_SET_MODULE =
struct
  (* Import *)

  open SyntaxModule
  open AnnotationModule


  (* Identifier sets *)

  type IdSet = {vids : VIdSet.set, strids : StrIdSet.set, funids : FunIdSet.set}

  local
    structure V = VIdSet
    structure S = StrIdSet
    structure F = FunIdSet
  in
    val empty = {vids = V.empty, strids = S.empty, funids = F.empty}

    fun fromVIds vids =
          {vids = V.fromList vids, strids = S.empty, funids = F.empty}
    fun fromStrIds strids =
          {vids = V.empty, strids = S.fromList strids, funids = F.empty}
    fun fromFunIds funids =
          {vids = V.empty, strids = S.empty, funids = F.fromList funids}
    fun fromCore {vids, strids} =
          {vids = vids, strids = strids, funids = F.empty}

    fun {vids = vids1, strids = strids1, funids = funids1} +
        {vids = vids2, strids = strids2, funids = funids2} =
          { vids   = V.union(vids1, vids2),
            strids = S.union(strids1, strids2),
            funids = F.union(funids1, funids2) }
    fun {vids = vids1, strids = strids1, funids = funids1} -
        {vids = vids2, strids = strids2, funids = funids2} =
          { vids   = VIdSet.difference(vids1, vids2),
            strids = StrIdSet.difference(strids1, strids2),
            funids = FunIdSet.difference(funids1, funids2) }

    fun disjoint(
          {vids = vids1, strids = strids1, funids = funids1},
          {vids = vids2, strids = strids2, funids = funids2}
        ) =
          VIdSet.disjoint(vids1, vids2) andalso
          StrIdSet.disjoint(strids1, strids2) andalso
          FunIdSet.disjoint(funids1, funids2)
  end

  fun boundEnv E = fromCore(IdSetCore.boundEnv E)
  fun boundFunEnv F = fromFunIds(FunIdMap.listKeys F)
  fun boundBasis(T, F, G, E) = boundFunEnv F + boundEnv E


  (* Free identifiers *)

  fun ?free NONE    = empty
    | ?free(SOME x) = free x

  fun freeFunId(funid@@_) = fromFunIds[funid]


  fun freeStrExp(STRUCTStrExp(strdec)@@A) =
        freeStrDec strdec
    | freeStrExp(IDStrExp(longstrid)@@_) =
        fromCore(IdSetCore.freeLongStrId longstrid)
    | freeStrExp(COLONStrExp(strexp, sigexp)@@_) =
        freeStrExp strexp
    | freeStrExp(SEALStrExp(strexp, sigexp)@@_) =
        freeStrExp strexp
    | freeStrExp(APPStrExp(funid, strexp)@@_) =
        freeFunId funid + freeStrExp strexp
    | freeStrExp(LETStrExp(strdec as _@@A1, strexp)@@_) =
        freeStrDec strdec + (freeStrExp strexp - boundEnv(get(elab A1)))

  and freeStrDec(DECStrDec(dec)@@_) =
        fromCore(IdSetCore.freeDec dec)
    | freeStrDec(STRUCTUREStrDec(strbind)@@_) =
        freeStrBind strbind
    | freeStrDec(LOCALStrDec(strdec1 as _@@A1, strdec2)@@_) =
        freeStrDec strdec1 + (freeStrDec strdec2 - boundEnv(get(elab A1)))
    | freeStrDec(EMPTYStrDec@@_) =
        empty
    | freeStrDec(SEQStrDec(strdec1 as _@@A1, strdec2)@@_) =
        freeStrDec strdec1 + (freeStrDec strdec2 - boundEnv(get(elab A1)))

  and freeStrBind(StrBind(strid, strexp, strbind_opt)@@_) =
        freeStrExp strexp + ?freeStrBind strbind_opt


  and freeFunDec(FunDec(funbind)@@_) =
        freeFunBind funbind

  and freeFunBind(FunBind(funid, strid@@_, sigexp, strexp, funbind_opt)@@_) =
        freeStrExp strexp - fromStrIds[strid]


  fun freeTopDec(STRDECTopDec(strdec as _@@A1, topdec_opt)@@_) =
        freeStrDec strdec + (?freeTopDec topdec_opt - boundEnv(get(elab A1)))
    | freeTopDec(SIGDECTopDec(sigdec, topdec_opt)@@_) =
        ?freeTopDec topdec_opt
    | freeTopDec(FUNDECTopDec(fundec as _@@A1, topdec_opt)@@_) =
        freeFunDec fundec + (?freeTopDec topdec_opt - boundFunEnv(get(elab A1)))


  (* Bound identifiers *)

  fun ?bound NONE    = []
    | ?bound(SOME x) = bound x

  fun boundStrBind(StrBind(strid@@_, strexp, strbind_opt)@@_) =
        strid :: ?boundStrBind strbind_opt

  fun boundFunBind(FunBind(funid@@_, strid, sigexp, strexp, funbind_opt)@@_) =
        funid :: ?boundFunBind funbind_opt
end;
