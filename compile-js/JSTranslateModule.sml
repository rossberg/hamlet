(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of module language into JavaScript.
 *)

structure JSTranslateModule : JS_TRANSLATE_MODULE =
struct
  (* Import *)

  type TopDec  = SyntaxModule.TopDec

  type var     = JSSyntax.var
  type expr    = JSSyntax.expr
  type stmt    = JSSyntax.stmt

  type Basis   = StaticObjectsModule.Basis

  structure ML = SyntaxModule
  structure A  = AnnotationModule
  structure JS = JSSyntax

  datatype phrase = datatype Annotation.phrase


  (* Helpers *)

  fun ?translate NONE    = []
    | ?translate(SOME x) = translate x


  (* Structures *)

  fun translateStrExp(ML.STRUCTStrExp(strdec)@@A) =
        (* (function() { strdec[[return { pack[[., E]] }]] })() *)
        JS.DoExpr(
          translateStrDec strdec
            (JS.RetStmt(JS.ObjExpr(packEnv NONE (A.get(A.elab A)))) :: [])
        )
    | translateStrExp(ML.IDStrExp(longstrid)@@_) =
        JSTranslateId.translateLongStrId longstrid
    | translateStrExp(ML.COLONStrExp(strexp, sigexp)@@A) =
      let
        val var = JS.inventVar()
      in
        (* (function(x) { return { pack[[x, E]] } })(strexp) *)
        JS.CallExpr(
          JS.FunExpr([var],
            JS.RetStmt(JS.ObjExpr(
              packEnv (SOME(JS.VarExpr(var))) (A.get(A.elab A)))
            ) :: []
          ),
          [translateStrExp strexp]
        )
      end
    | translateStrExp(ML.SEALStrExp(strexp, sigexp)@@A) =
      let
        val var = JS.inventVar()
      in
        (* (function(x) { return { pack[[x, E]] } })(strexp) *)
        JS.CallExpr(
          JS.FunExpr([var],
            JS.RetStmt(JS.ObjExpr(
              packEnv (SOME(JS.VarExpr(var))) (A.get(A.elab A)))
            ) :: []
          ),
          [translateStrExp strexp]
        )
      end
    | translateStrExp(ML.APPStrExp(funid, strexp)@@_) =
        (* funid(strexp) *)
        JS.CallExpr(
          JS.VarExpr(JSTranslateId.translateFunId funid),
          [translateStrExp strexp]
        )
    | translateStrExp(ML.LETStrExp(strdec, strexp)@@_) =
        (* (function() { [[strdec]][return [[exp]]] })() *)
        JS.DoExpr(
          translateStrDec strdec (JS.RetStmt(translateStrExp strexp) :: [])
        )

  and translateStrDec(ML.DECStrDec(dec)@@_) ret =
        JSTranslateCore.translateDec dec ret
    | translateStrDec(ML.STRUCTUREStrDec(strbind)@@_) ret =
      let
        val strids = IdSetModule.boundStrBind strbind
        val vars   = List.map (JS.inventVar o ignore) strids
        val bound  = StrIdSet.fromList strids
        val free   = #strids(IdSetModule.freeStrBind strbind)
      in
        if StrIdSet.disjoint(bound, free) then
          (* var strid1 = strexp; ...; var stridN = strexpN; ret *)
          translateRhsStrBind
            (List.map JSTranslateId.translateStrId' strids) strbind @
          ret
        else
          (* var x1 = strexp1; ...; var xN = strexpN;
           * return (function() { var strid1 = x1; ...; stridN = xN; ret })
           *)
          translateRhsStrBind vars strbind @
          JS.RetStmt(JS.DoExpr(translateLhsStrBind vars strbind @ ret)) :: []
      end
    | translateStrDec(ML.LOCALStrDec(strdec1, strdec2)@@A) ret =
        (* nest[[strdec1]][[strdec2]][[E]]; ret *)
        JSTranslateCore.nestDec(A.get(A.elab A),
          translateStrDec strdec1, translateStrDec strdec2
        ) @ ret
    | translateStrDec(ML.EMPTYStrDec@@_) ret =
        ret
    | translateStrDec(ML.SEQStrDec(strdec1 as _@@A1, strdec2 as _@@A2)@@A) ret =
        if
          StaticEnv.disjoint(A.get(A.elab A1), A.get(A.elab A2)) andalso
          IdSetModule.disjoint(
            IdSetModule.freeStrDec strdec1,
            IdSetModule.boundEnv(A.get(A.elab A2))
          )
        then
          (* strdec1[[strdec2[[ret]]] *)
          translateStrDec strdec1 (translateStrDec strdec2 ret)
        else
          (* strdec1[[return (function() { strdec2[[ret]] })()]] *)
          translateStrDec strdec1
            [JS.RetStmt(JS.DoExpr(translateStrDec strdec2 ret))]

  and translateRhsStrBind vars (ML.StrBind(strid, strexp, strbind_opt)@@_) =
        (* var x = strexp; strbind_opt *)
        JS.VarStmt(List.hd vars, translateStrExp strexp) ::
        ?(translateRhsStrBind(List.tl vars)) strbind_opt

  and translateLhsStrBind vars (ML.StrBind(strid, strexp, strbind_opt)@@_) =
        (* var strid = x; strbind_opt *)
        JS.VarStmt(JSTranslateId.translateStrId strid,
          JS.VarExpr(List.hd vars)
        ) :: ?(translateLhsStrBind(List.tl vars)) strbind_opt


  (* Functors *)

  and translateFunDec(ML.FunDec(funbind)@@_) ret =
      let
        val funids = IdSetModule.boundFunBind funbind
        val vars   = List.map (JS.inventVar o ignore) funids
        val bound  = FunIdSet.fromList funids
        val free   = #funids(IdSetModule.freeFunBind funbind)
      in
        if FunIdSet.disjoint(bound, free) then
          (* function funid1() {...}; ...; function funidN() {...}; ret *)
          translateRhsFunBind (List.map JSTranslateId.translateFunId' funids)
            funbind @ ret
        else
          (* function x1() {...}; ...; function xN() {...};
             return (function() { var funid1 = x1; ...; funidN = xN; ret }) *)
          translateRhsFunBind vars funbind @
          JS.RetStmt(JS.DoExpr(translateLhsFunBind vars funbind @ ret)) :: []
      end

  and translateRhsFunBind vars (
        ML.FunBind(funid, strid, sigexp, strexp, funbind_opt)@@_
      ) =
        (* function x(strid) { return strexp }; funbind_opt *)
        JS.FunStmt(
          List.hd vars, [JSTranslateId.translateStrId strid],
          JS.RetStmt(translateStrExp strexp) :: []
        ) :: ?(translateRhsFunBind(List.tl vars)) funbind_opt

  and translateLhsFunBind vars (
        ML.FunBind(funid, strid, sigexp, strexp, funbind_opt)@@_
      ) =
        (* var funid = x; funbind_opt *)
        JS.VarStmt(JSTranslateId.translateFunId funid,
          JS.VarExpr(List.hd vars)
        ) :: ?(translateLhsFunBind(List.tl vars)) funbind_opt


  (* Top-level declarations *)

  and packEnv expr_opt E =
        JSTranslateCore.packEnv expr_opt E
  and packBasis expr_opt (T, F, G, E) =
        packFunEnv expr_opt F @ packEnv expr_opt E
  and packFunEnv expr_opt F =
        (* 'funid1': x.funid1, ..., 'funidN': x.funidN *)
        FunIdMap.foldri
          (fn(funid, _, props) =>
            JSTranslateCore.packVar expr_opt
              (JSTranslateId.translateFunId' funid) :: props
          ) [] F

  and openEnv expr E = JSTranslateCore.openEnv expr E
  and openBasis expr (T, F, G, E) = openFunEnv expr F @ openEnv expr E
  and openFunEnv expr F =
        (* var funid1 = x.funid1; ...; var funidN = x.funidN *)
        FunIdMap.foldri
          (fn(funid, _, stmts) =>
            JS.VarStmt(JSTranslateId.translateFunId' funid,
              JS.DotExpr(expr, JSTranslateId.translateFunId' funid)
            ) :: stmts
          ) [] F

  and nestTopDec(B, makeStmts1, makeStmts2) =
      let
        val var = JS.inventVar()
      in
        (* var x = (function() {
         *   stmts1[[return (function() {
         *     stmts2[[return { pack[[., B]] }]]
         *   })()]]
         * })(); open[[x:B]]
         *)
        JS.VarStmt(var,
          JS.DoExpr(
            makeStmts1(
              JS.RetStmt(
                JS.DoExpr(
                  makeStmts2(JS.RetStmt(JS.ObjExpr(packBasis NONE B)) :: [])
                )
              ) :: []
            )
          )
        ) :: openBasis (JS.VarExpr(var)) B
      end

  fun translateTopDec(ML.STRDECTopDec(strdec as _@@A1, topdec_opt)@@A) ret =
        (case topdec_opt of
          NONE => translateStrDec strdec ret
        | SOME(topdec as _@@A2) =>
          if
            StaticEnv.disjoint(A.get(A.elab A1), #4(A.get(A.elab A2))) andalso
            IdSetModule.disjoint(
              IdSetModule.freeStrDec strdec,
              IdSetModule.boundBasis(A.get(A.elab A))
            )
          then
            (* strdec[[topdec_opt[[ret]]]] *)
            translateStrDec strdec (translateTopDec topdec ret)
          else
            (* strdec[[return (function() { topdec_opt[[ret]] })()]] *)
            translateStrDec strdec
              [JS.RetStmt(JS.DoExpr(translateTopDec topdec ret))]
        )
    | translateTopDec(ML.SIGDECTopDec(sigdec, topdec_opt)@@_) ret =
        (case topdec_opt of
          NONE => ret
        | SOME topdec => translateTopDec topdec ret
        )
    | translateTopDec(ML.FUNDECTopDec(fundec as _@@A1, topdec_opt)@@A) ret =
        (case topdec_opt of
          NONE => translateFunDec fundec ret
        | SOME(topdec as _@@A2) =>
          if
            FunIdMap.disjoint(A.get(A.elab A1), #2(A.get(A.elab A2))) andalso
            IdSetModule.disjoint(
              IdSetModule.freeFunDec fundec,
              IdSetModule.boundBasis(A.get(A.elab A))
            )
          then
            (* fundec[[topdec[[ret]]]] *)
            translateFunDec fundec (translateTopDec topdec ret)
          else
            (* fundec[[return (function() { topdec_opt[[ret]] })()]] *)
            translateFunDec fundec
              [JS.RetStmt(JS.DoExpr(translateTopDec topdec ret))]
        )
end;
