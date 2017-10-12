(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of core language into JavaScript.
 *)

structure JSTranslateCore : JS_TRANSLATE_CORE =
struct
  (* Import *)

  type Dec  = SyntaxCore.Dec

  type name = JSSyntax.name
  type var  = JSSyntax.var
  type expr = JSSyntax.expr
  type stmt = JSSyntax.stmt

  type Env  = StaticObjectsCore.Env

  structure ML = SyntaxCore
  structure A  = AnnotationCore
  structure JS = JSSyntax

  datatype phrase = datatype Annotation.phrase


  (* Helpers *)

  fun ?translate NONE    = []
    | ?translate(SOME x) = translate x

  datatype tuple_type = PLAIN | TUPLE of int | ABSTRACT

  fun asTupleRow(n, [])        = if n <> 2 then TUPLE(n - 1) else PLAIN
    | asTupleRow(n, lab::labs) =
        if lab = Lab.fromInt n then asTupleRow(n + 1, labs) else PLAIN

  fun asTupleType tau =
        case Type.determined tau of
          StaticObjectsCore.RowType(rho, r) =>
            asTupleRow(1, LabMap.listKeys rho)
        | StaticObjectsCore.ConsType(taus, t) =>
            if TyName.span t > 0 orelse TyNameSet.member(InitialStaticEnv.T0, t)
            then PLAIN else ABSTRACT
        | StaticObjectsCore.TyVar alpha => ABSTRACT
        | _ => PLAIN

  fun asSimpleLongVId(longvid@@A : ML.longVId) =
        if #2(A.get(A.elab A)) = IdStatus.v then
          SOME(LongVId.toId longvid)
        else
          NONE

  fun asSimpleAtPat(ML.WILDCARDAtPat@@_)              = NONE
    | asSimpleAtPat(ML.SCONAtPat(scon)@@_)            = NONE
    | asSimpleAtPat(ML.IDAtPat(_, longvid)@@_)        = asSimpleLongVId longvid
    | asSimpleAtPat(ML.RECORDAtPat(patrow_opt)@@_)    = NONE
    | asSimpleAtPat(ML.PARAtPat(pat)@@_)              = asSimplePat pat

  and asSimplePat(ML.ATPat(atpat)@@_)                 = asSimpleAtPat atpat
    | asSimplePat(ML.CONPat(_, longvid@@_, atpat)@@_) = NONE
    | asSimplePat(ML.COLONPat(pat, ty)@@_)            = asSimplePat pat
    | asSimplePat(ML.ASPat(_, vid, ty_opt, pat)@@_)   = NONE

  fun asSimpleMrule(ML.Mrule(pat, exp)@@_) =
        Option.map (fn vid => (vid, exp)) (asSimplePat pat)

  fun asSimpleMatch(ML.Match(mrule, NONE)@@_)       = asSimpleMrule mrule
    | asSimpleMatch(ML.Match(mrule, SOME match)@@_) = NONE

  fun isSimpleValBind(ML.PLAINValBind(pat, exp, NONE)@@_) =
        asSimplePat pat <> NONE
    | isSimpleValBind(ML.PLAINValBind(pat, exp, SOME valbind)@@_) =
        asSimplePat pat <> NONE andalso isSimpleValBind valbind
    | isSimpleValBind(ML.RECValBind(valbind)@@_) = true


  (* Internal identifiers *)

  val failExpr = JS.StringExpr("FAIL")
  val jsExpr = JS.VarExpr("_JS")
  val smlExpr = JS.VarExpr("_SML")
  val errorExpr = JS.DotExpr(jsExpr, "Error")
  val bindExpr = JS.DotExpr(smlExpr, "Bind")
  val matchExpr = JS.DotExpr(smlExpr, "Match")
  val isExnExpr = JS.DotExpr(smlExpr, "_isExn")
  val isTupleExpr = JS.DotExpr(smlExpr, "_isTuple")
  val tuplifyArgsExpr = JS.DotExpr(smlExpr, "_tuplifyArgs")
  val checkOverflowExpr = JS.DotExpr(smlExpr, "_checkOverflow")
  val unsignedExpr = JS.DotExpr(JS.DotExpr(smlExpr, "Word"), "_toInt")


  (* Helpers for translating operators *)

  fun unop "abs"  = NONE
    | unop "~"    = SOME JS.NegOp
    | unop _      = NONE

  fun binop "+"   = SOME JS.AddOp
    | binop "-"   = SOME JS.SubOp
    | binop "*"   = SOME JS.MulOp
    | binop "div" = SOME JS.DivOp
    | binop "mod" = SOME JS.ModOp
    | binop "/"   = SOME JS.DivOp
    | binop "<"   = SOME JS.LessOp
    | binop ">"   = SOME JS.GreaterOp
    | binop "<="  = SOME JS.LessEqOp
    | binop ">="  = SOME JS.GreaterEqOp
    | binop "="   = SOME JS.EqOp
    | binop _     = NONE

  fun unsigned JS.DivOp       = true
    | unsigned JS.MulOp       = true
    | unsigned JS.LessOp      = true
    | unsigned JS.GreaterOp   = true
    | unsigned JS.LessEqOp    = true
    | unsigned JS.GreaterEqOp = true
    | unsigned _              = false

  fun coerce(t, expr) =
        if t = InitialStaticEnv.tInt then
          (* _SML._checkOverflow(expr) *)
          JS.CallExpr(checkOverflowExpr, [expr])
        else if t = InitialStaticEnv.tWord then
          (* expr | 0 *)
          JS.BinExpr(JS.BitOrOp, expr, JS.NumExpr("0"))
        else if t = StaticLibrary.tWord8 then
          (* expr & 0xff *)
          JS.BinExpr(JS.BitAndOp, expr, JS.NumExpr("0xff"))
        else
          expr


  (* Helpers for translating calls *)

  fun asTupleExpRow 2 NONE = NONE
    | asTupleExpRow i NONE = SOME []
    | asTupleExpRow i (SOME(ML.ExpRow(lab@@_, exp, exprow_opt)@@_)) =
        if Lab.toString lab <> Int.toString i then NONE else
        case asTupleExpRow (i + 1) exprow_opt of
          SOME exps => SOME(exp::exps)
        | NONE      => NONE

  fun argumentAtExp(atexp as ML.RECORDAtExp(exprow_opt)@@_) =
        (case asTupleExpRow 1 exprow_opt of
          SOME exps => exps
        | NONE => [ML.ATExp(atexp)@@A.at(atexp)]
        )
    | argumentAtExp atexp = [ML.ATExp(atexp)@@A.at(atexp)]

  datatype call =
    UNARY  of JS.unop * ML.Exp * TyName.TyName
  | BINARY of JS.binop * ML.Exp * ML.Exp * TyName.TyName
  | CALL   of ML.Exp list
  | DOT    of ML.Lab * ML.AtExp

  fun call(ML.ATExp(ML.IDAtExp(_, longvid@@A')@@A)@@_, atexp) =
      let
        val exps = argumentAtExp atexp
      in
        case #1(A.get(A.elab A')) of
          sigma as ([alpha], tau) =>
          let
            val vid  = VId.toString(LongVId.toId longvid)
          in
            if Option.isSome(TyVar.overloadingClass alpha) then
              case (unop vid, binop vid, exps) of
                (SOME unop, NONE, [exp]) =>
                  UNARY(unop, exp, Type.tyname(A.get(A.elab A)))
              | (NONE, SOME binop, [exp1, exp2]) =>
                  BINARY(binop, exp1, exp2, Type.tyname(A.get(A.elab A)))
              | _ => CALL exps
            else if vid = "=" then
              case (binop vid, exps) of
                (SOME binop, [exp1, exp2]) =>
                  BINARY(binop, exp1, exp2, Type.tyname(A.get(A.elab A)))
              | _ => CALL exps
            else CALL exps
          end
        | _ => CALL exps
      end
    | call(exp as
        ML.ATExp(ML.PARAtExp(ML.FNExp(ML.Match(ML.Mrule(
          ML.ATPat(ML.RECORDAtPat(SOME(ML.FIELDPatRow(
            lab, ML.ATPat(ML.IDAtPat(_, longvid1@@_)@@_)@@_,
            SOME(ML.DOTSPatRow@@_)
          )@@_))@@_)@@_,
          ML.ATExp(ML.IDAtExp(_, longvid2@@_)@@_)@@_)@@_,
          NONE)@@_)@@_)@@_)@@_,
        atexp
      ) =
        if
          LongVId.isShort longvid1 andalso LongVId.isShort longvid2 andalso
          LongVId.toId longvid1 = LongVId.toId longvid2
        then
          DOT(lab, atexp)
        else
          CALL(argumentAtExp atexp)
    | call(exp, atexp) =
          CALL(argumentAtExp atexp)


  (* Helpers for translating functions *)

  fun etaApp(ML.APPExp(exp, atexp)@@_) =
        (case (etaFn exp, etaAtArg atexp) of
          (SOME match, SOME vid) => SOME(match, vid)
        | _ => NONE
        )
    | etaApp(ML.ATExp(atexp)@@_)          = etaAtApp atexp
    | etaApp _                            = NONE
  and etaAtApp(ML.PARAtExp(exp)@@_)       = etaApp exp
    | etaAtApp _                          = NONE

  and etaFn(ML.FNExp(match)@@_)           = SOME match
    | etaFn(ML.ATExp(atexp)@@_)           = etaAtFn atexp
    | etaFn _                             = NONE
  and etaAtFn(ML.PARAtExp(exp)@@_)        = etaFn exp
    | etaAtFn _                           = NONE

  and etaArg(ML.ATExp(atexp)@@_)          = etaAtArg atexp
    | etaArg _                            = NONE
  and etaAtArg(ML.IDAtExp(_, longvid)@@_) = asSimpleLongVId longvid
    | etaAtArg(ML.PARAtExp(exp)@@_)       = etaArg exp
    | etaAtArg _                          = NONE

  fun etaMatch(match as ML.Match(ML.Mrule(pat, exp)@@_, NONE)@@_) =
        (case (asSimplePat pat, etaApp exp) of
          (SOME vid1, SOME(match', vid2)) =>
            if vid1 = vid2 andalso VId.isInvented vid1 then match' else match
        | _ => match
        )
    | etaMatch match = match

  fun asTupleAtPat(ML.RECORDAtPat(patrow_opt)@@_) = asTuplePatRow 1 patrow_opt
    | asTupleAtPat(ML.PARAtPat(pat)@@_)           = asTuplePat pat
    | asTupleAtPat _                              = NONE
  and asTuplePat(ML.ATPat(atpat)@@_)              = asTupleAtPat atpat
    | asTuplePat _                                = NONE
  and asTuplePatRow 2 NONE                        = NONE
    | asTuplePatRow i NONE                        = SOME []
    | asTuplePatRow i (SOME(ML.DOTSPatRow@@_))    = NONE
    | asTuplePatRow i (SOME(ML.FIELDPatRow(lab@@_, pat, patrow_opt)@@_)) =
        if Lab.toString lab <> Int.toString i then NONE else
        case asTuplePatRow (i + 1) patrow_opt of
          SOME pats => SOME(pat::pats)
        | NONE      => NONE

  datatype function =
    FUNCTION of VId.Id list * (VId.Id * ML.Pat) list * ML.Exp
  | MATCH    of ML.Match

  fun function match =
        case etaMatch match of
          match' as ML.Match(ML.Mrule(pat, exp)@@_, NONE)@@A =>
            (case (asTuplePat pat, A.get(A.exhaustive A)) of
              (SOME pats, StaticObjectsCore.Exhaustive) =>
              let
                val params = List.map (
                    fn pat =>
                      case asSimplePat pat of
                        SOME vid => (vid, NONE)
                      | NONE =>
                        let val vid = VId.invent() in (vid, SOME(vid, pat)) end
                  ) pats
              in
                FUNCTION(List.map #1 params, List.mapPartial #2 params, exp)
              end
            | _ => MATCH match'
            )
        | match' => MATCH match'


  (* Expressions *)

  fun translateAtExp(ML.SCONAtExp(scon)@@_) =
        JSTranslateSCon.translateSCon scon
    | translateAtExp(ML.IDAtExp(_, longvid@@A')@@A) =
        (case #1(A.get(A.elab A')) of
          sigma as ([alpha], tau) =>
            if Option.isSome(TyVar.overloadingClass alpha) then
              let
                val (taus, tau) = TypeScheme.instance sigma
              in
                Type.unify(tau, A.get(A.elab A));
                (* _SML.vid.tyname *)
                JS.DotExpr(
                  JS.DotExpr(smlExpr, VId.toString(LongVId.toId longvid)),
                  TyName.toString(Type.tyname(List.hd taus))
                )
              end
            else JSTranslateId.translateLongVId(longvid@@A')
        | _ => JSTranslateId.translateLongVId(longvid@@A')
        )
    | translateAtExp(ML.RECORDAtExp(exprow_opt)@@_) =
        (case asTupleExpRow 1 exprow_opt of
          NONE      => JS.ObjExpr(?translateExpRow exprow_opt)
        | SOME []   => JS.UndefExpr
        | SOME exps => JS.ArrExpr(List.map (SOME o translateExp) exps)
        )
    | translateAtExp(ML.LETAtExp(dec, exp)@@_) =
        (* (function() { dec[[return exp]] })() *)
        JS.DoExpr(translateDec dec (JS.RetStmt(translateExp exp) :: []))
    | translateAtExp(ML.PARAtExp(exp)@@_) =
        translateExp exp

  and translateExpRow(ML.ExpRow(lab, exp, exprow_opt)@@_) =
        (JSTranslateId.translateLab lab, translateExp exp) ::
        ?translateExpRow exprow_opt

  and translateExp(ML.ATExp(atexp)@@_) =
        translateAtExp atexp
    | translateExp(ML.APPExp(exp, atexp as _@@A)@@_) =
        (case call(exp, atexp) of
          UNARY(unop, exp, t) =>
            (* coerce[[@exp1]][[t]] *)
            coerce(t, JS.UnExpr(unop, translateExp exp))
        | BINARY(binop, exp1, exp2, t) =>
            if t <> InitialStaticEnv.tWord orelse not(unsigned binop) then
              (* coerce[[exp1 @ exp2]][[t]] *)
              coerce(t, JS.BinExpr(binop, translateExp exp1, translateExp exp2))
            else
              (* coerce[[_SML.Word._toInt(exp1) @
               *         _SML.Word._toInt(exp2)]][[t]] *)
              coerce(t,
                JS.BinExpr(binop,
                  JS.CallExpr(unsignedExpr, [translateExp exp1]),
                  JS.CallExpr(unsignedExpr, [translateExp exp2])
                )
              )
        | DOT(lab, atexp) =>
            JS.DotExpr(translateAtExp atexp, JSTranslateId.translateLab lab)
        | CALL [exp1] =>
            (case asTupleType(A.get(A.elab A)) of
              PLAIN =>
                (* exp(exp1) *)
                JS.CallExpr(translateExp exp, [translateExp exp1])
            | TUPLE 0 =>
                (* exp(exp1) *)
                JS.CallExpr(translateExp exp, [translateExp exp1])
            | TUPLE n =>
                (* exp.apply(undefined, exp1) *)
                JS.CallExpr(
                  JS.DotExpr(translateExp exp, "apply"),
                  [JS.UndefExpr, translateExp exp1]
                )
            | ABSTRACT =>
              (* (function() {
               *   var f = exp; var x = exp1;
               *   return _SML._isTuple(x) ? f.apply(undefined, x) : f(x);
               * })()
               *)
              let
                val var  = JS.inventVar()
                val var1 = JS.inventVar()
              in
                JS.DoExpr(
                  JS.VarStmt(var, translateExp exp) ::
                  JS.VarStmt(var1, translateExp exp1) ::
                  JS.RetStmt(
                    JS.IfExpr(
                      JS.CallExpr(isTupleExpr, [JS.VarExpr(var1)]),
                      JS.CallExpr(
                        JS.DotExpr(JS.VarExpr(var), "apply"),
                        [JS.UndefExpr, JS.VarExpr(var1)]
                      ),
                      JS.CallExpr(JS.VarExpr(var), [JS.VarExpr(var1)])
                    )
                  ) :: []
                )
              end
            )
        | CALL exps =>
            (* exp(exp1, ..., expN) *)
            JS.CallExpr(translateExp exp, List.map translateExp exps)
        )
    | translateExp(ML.COLONExp(exp, ty)@@_) =
        translateExp exp
    | translateExp(ML.HANDLEExp(exp, match)@@_) =
      let
        val (var, stmts) =
            case asSimpleMatch match of
              SOME(vid, exp2) =>
                ( JSTranslateId.translateVId' vid,
                  JS.RetStmt(translateExp exp2) :: [] )
            | NONE =>
              let val var = JS.inventVar() in
                (var, translateMatch (var, JS.VarExpr(var)) match)
              end
      in
        (* (function() {
         *   try { return exp } catch(x) {
         *     if (!_SML._isExn(x)) throw x
         *     match[[x, x]]
         *   }
         * })()
         *)
        JS.DoExpr(
          JS.TryStmt(
            JS.RetStmt(translateExp exp) :: [],
            var,
            JS.IfStmt(
              JS.UnExpr(JS.NotOp, JS.CallExpr(isExnExpr, [JS.VarExpr(var)])),
              JS.ThrowStmt(JS.VarExpr(var)),
              NONE
            ) :: stmts
          ) :: []
        )
      end
    | translateExp(ML.RAISEExp(exp)@@_) =
        (* (function() { throw exp })() *)
        JS.DoExpr(JS.ThrowStmt(translateExp exp) :: [])
    | translateExp(ML.FNExp(match)@@A) =
        (case function match of
          FUNCTION(vids, vid_pats, exp) => 
            (* function(vid1, ..., vidN) {
             *   pat1[[vidK1]]; ...; patM[[vidKM]]; return exp
             * }
             *)
            JS.FunExpr(List.map JSTranslateId.translateVId' vids,
              List.concat (
                List.map (fn(vid, pat) =>
                  translatePat (JSTranslateId.translateVId' vid, failExpr) pat
                ) vid_pats
              ) @ JS.RetStmt(translateExp exp) :: []
            )
        | MATCH match' =>
          let
            val var = JS.inventVar()
          in
            (case asTupleType(#1(Type.toFunType(A.get(A.elab A)))) of
              PLAIN =>
                (* function(x) { match'[[x]][[throw _SML.Match]] } *)
                JS.FunExpr([var], translateMatch (var, matchExpr) match')
            | _ =>
                (* function() {
                 *   var x = _SML._tuplifyArgs(arguments);
                 *   match'[[x]][[throw _SML.Match]]
                 * }
                 *)
                JS.FunExpr([],
                  JS.VarStmt(var,
                    JS.CallExpr(tuplifyArgsExpr, [JS.VarExpr("arguments")])
                  ) :: translateMatch (var, matchExpr) match'
                )
            )
          end
        )


  (* Matches *)

  and translateMatch (var, exn) (ML.Match(mrule, NONE)@@A) =
        (* mrule[[x]] *)
        translateMrule (var, exn) mrule
    | translateMatch (var, exn) (ML.Match(mrule, SOME match)@@A) =
        (* try { mrule[[x]] } catch(x') {
         *   if (x' !== "FAIL") throw x'; match_opt[[x]]; throw exn
         * }
         *)
        let
          val var' = JS.inventVar()
        in
          JS.TryStmt(
            translateMrule (var, failExpr) mrule,
            var',
            JS.IfStmt(
              JS.BinExpr(JS.NeqOp, JS.VarExpr(var'), failExpr),
              JS.ThrowStmt(JS.VarExpr(var')),
              NONE
            ) :: translateMatch (var, exn) match
          ) :: []
        end

  and translateMrule (var, exn) (ML.Mrule(pat, exp)@@_) =
        (* return (function() { pat[[x]]; return exp })() *)
        JS.RetStmt(JS.DoExpr(
          translatePat (var, exn) pat @
          JS.RetStmt(translateExp exp) :: []
        )) :: []


  (* Declarations *)

  and packVar NONE var = (var, JS.VarExpr(var))
    | packVar (SOME expr) var = (var, JS.DotExpr(expr, var))

  and packEnv expr_opt (StaticObjectsCore.Env(SE, TE, VE)) =
        packStrEnv expr_opt SE @ packValEnv expr_opt VE
  and packValEnv expr_opt VE =
        (* 'vid1': x.vid1, ..., 'vidN': x.vidN *)
        VIdMap.foldri
          (fn(vid, _, props) =>
            packVar expr_opt (JSTranslateId.translateVId' vid) :: props
          ) [] VE
  and packStrEnv expr_opt SE =
        (* 'strid1': x.strid1, ..., 'stridN': x.stridN *)
        StrIdMap.foldri
          (fn(strid, _, props) =>
            packVar expr_opt (JSTranslateId.translateStrId' strid) :: props
          ) [] SE

  and openEnv expr (StaticObjectsCore.Env(SE, TE, VE)) =
        openStrEnv expr SE @ openValEnv expr VE
  and openValEnv expr VE =
        (* var vid1 = expr.vid1; ...; var vidN = expr.vidN *)
        VIdMap.foldri
          (fn(vid, _, stmts) =>
            let
              val id = JSTranslateId.translateVId' vid
            in
              if id = "true" orelse id = "false" then
                JS.ExprStmt(JS.StringExpr(id))
              else
                JS.VarStmt(JSTranslateId.translateVId' vid,
                  JS.DotExpr(expr, JSTranslateId.translateVId' vid)
                )
            end :: stmts
          ) [] VE
  and openStrEnv expr SE =
        (* var strid1 = expr.strid1; ...; var stridN = expr.stridN *)
        StrIdMap.foldri
          (fn(strid, _, stmts) =>
            JS.VarStmt(JSTranslateId.translateStrId' strid,
              JS.DotExpr(expr, JSTranslateId.translateStrId' strid)
            ) :: stmts
          ) [] SE

  and openConEnv VE =
        VIdMap.foldri
          (fn(vid, ((alphas, tau), is), stmts) =>
            (case !tau of
              (* var vid = 'vid' *)
              StaticObjectsCore.ConsType _ =>
              let
                val id = JSTranslateId.translateVId' vid
              in
                if id = "true" orelse id = "false" then
                  JS.ExprStmt(JS.StringExpr(id))
                else
                  JS.VarStmt(JSTranslateId.translateVId' vid,
                    JS.StringExpr(VId.toString vid)
                  )
              end
            | StaticObjectsCore.FunType(tau1, _) =>
                (case asTupleType tau1 of
                  PLAIN =>
                  let
                    val var' = JS.inventVar()
                  in
                    (* function vid(x) { return {'vid': x}; } *)
                    JS.FunStmt(JSTranslateId.translateVId' vid, [var'],
                      JS.RetStmt(
                        JS.ObjExpr[(VId.toString vid, JS.VarExpr(var'))]
                      ) :: []
                    )
                  end
                | _ =>
                    (* function vid() {
                     *   return {'vid': _SML._tuplifyArgs(arguments)};
                     * }
                     *)
                    JS.FunStmt(JSTranslateId.translateVId' vid, [],
                      JS.RetStmt(
                        JS.ObjExpr[
                          ( VId.toString vid,
                            JS.CallExpr(
                              tuplifyArgsExpr, [JS.VarExpr("arguments")]
                            )
                          )
                        ]
                      ) :: []
                    )
                )
            | _ => raise Fail "JSTranslateCore.openConEnv: invalid constructor"
            ) :: stmts
          ) [] VE

  and nestDec(E, makeStmts1, makeStmts2) =
      let
        val var = JS.inventVar()
      in
        (* var x = (function() {
         *   stmts1[[return (function() {
         *     stmts2[[return { pack[[., E]] }]]
         *   })()]]
         * })();
         * open[[x:E]]
         *)
        JS.VarStmt(var,
          JS.DoExpr(
            makeStmts1(
              JS.RetStmt(
                JS.DoExpr(
                  makeStmts2(JS.RetStmt(JS.ObjExpr(packEnv NONE E)) :: [])
                )
              ) :: []
            )
          )
        ) :: openEnv (JS.VarExpr(var)) E
      end

  and translateDec(ML.VALDec(tyvarseq, valbind)@@_) ret =
      let
        val bounds = IdSetCore.boundValBind valbind
        val bound  = List.foldl VIdSet.union VIdSet.empty bounds
        val free   = #vids(IdSetCore.freeValBind valbind)
        val vars   = List.map (JS.inventVar o ignore) bounds
      in
        if VIdSet.disjoint(bound, free) then
          if isSimpleValBind valbind then
            (* var vid1 = exp1; ...; vidN = expN;
             * var vidrec1 = exprec1; ...; var vidrecN = exprecN;
             * ret
             *)
            translateRhsValBind
              (List.map (JSTranslateId.translateVId' o valOf o VIdSet.first)
                bounds) valbind @
            translateRecValBind vars false valbind @ ret
          else
            (* var x1 = exp1; ...; xN = expN;
             * pat1[[x1]] ... patN[[xN]]
             * var vidrec1 = exprec1; ...; var vidrecN = exprecN;
             * ret
             *)
            translateRhsValBind vars valbind @
            translateLhsValBind vars valbind @
            translateRecValBind vars false valbind @ ret
        else
          (* var x1 = exp1; ...; xN = expN;
           * return (function() {
           *   var vidrec1 = exprec1; ...; var vidrecN = exprecN;
           *   return (function() { pat1[[x1]] ... patN[[xN]]; ret })()
           * })()
           *)
          translateRhsValBind vars valbind @
          JS.RetStmt(
            JS.DoExpr(
              case translateRecValBind vars false valbind of
                [] =>
                translateLhsValBind vars valbind @ ret
              | stmts =>
                stmts @
                JS.RetStmt(
                  JS.DoExpr(translateLhsValBind vars valbind @ ret)
                ) :: []
            )
          ) :: []
      end
    | translateDec(ML.TYPEDec(typbind)@@_) ret =
        ret
    | translateDec(ML.DATATYPEDec(datbind@@A)@@_) ret =
        openConEnv (#1(A.get(A.elab A))) @ ret
    | translateDec(ML.DATATYPE2Dec(tycon, longtycon'@@A)@@_) ret =
        openConEnv (#2(A.get(A.elab A))) @ ret
    | translateDec(ML.ABSTYPEDec(datbind@@A', dec)@@A) ret =
        (* nest[[datbind]][[dec]][[E]]; ret *)
        nestDec(A.get(A.elab A),
          fn ret' => openConEnv (#1(A.get(A.elab A'))) @ ret',
          translateDec dec
        ) @ ret
    | translateDec(ML.EXCEPTIONDec(exbind)@@A) ret =
      let
        val bound = IdSetCore.boundExBind exbind
        val free  = #vids(IdSetCore.freeExBind exbind)
        val vars  = List.map (JS.inventVar o ignore) bound
      in
        if VIdSet.disjoint(VIdSet.fromList bound, free) then
          translateRhsExBind vars exbind @
          translateLhsExBind vars exbind @ ret
        else
          (* var x1 = exp1; ...; xN = expN;
           * return (function() { vid1 = x1 ... vidN = xN; ret })()
           *)
          translateRhsExBind vars exbind @
          JS.RetStmt(JS.DoExpr(translateLhsExBind vars exbind @ ret)) :: []
      end
    | translateDec(ML.LOCALDec(dec1, dec2)@@A) ret =
        (* nest[[dec1]][[dec2]][[E]]; ret *)
        nestDec(A.get(A.elab A), translateDec dec1, translateDec dec2) @ ret
    | translateDec(ML.OPENDec(longstrids)@@A) ret =
      let
        val bound = IdSetCore.boundEnv(A.get(A.elab A))
        val free  = IdSetCore.freeLongStrIds longstrids
        val vars  = List.map (JS.inventVar o ignore) longstrids
      in
        if IdSetCore.disjoint(bound, free) then
          (* open[[longstrid1:E1]] ... open[[longstridN:EN]]; ret *)
          ListPair.foldrEq
            (fn(longstrid as _@@A', var, stmts) =>
              openEnv (JSTranslateId.translateLongStrId longstrid)
                (A.get(A.elab A')) @ stmts
            ) [] (longstrids, vars) @
          ret
        else
          (* var x1 = longstrid1; ...; var xN = longstridN;
           * open[[x1:E1]] ... open[[xN:EN]];
           * ret
           *)
          ListPair.foldrEq
            (fn(longstrid, var, stmts) =>
              JS.VarStmt(var, JSTranslateId.translateLongStrId longstrid) ::
              stmts
            ) [] (longstrids, vars) @
          ListPair.foldrEq
            (fn(longstrid@@A, var, stmts) =>
              openEnv (JS.VarExpr(var)) (A.get(A.elab A)) @ stmts
            ) [] (longstrids, vars) @
          ret
      end
    | translateDec(ML.EMPTYDec@@_) ret =
        ret
    | translateDec(ML.SEQDec(dec1 as _@@A1, dec2 as _@@A2)@@A) ret =
        if
          StaticEnv.disjoint(A.get(A.elab A1), A.get(A.elab A2)) andalso
          IdSetCore.disjoint(
            IdSetCore.freeDec dec1, IdSetCore.boundEnv(A.get(A.elab A2)))
        then
          (* dec1[[dec2[[ret]]]] *)
          translateDec dec1 (translateDec dec2 ret)
        else
          (* dec1[[return (function() { dec2[[ret]] })()]] *)
          translateDec dec1 [JS.RetStmt(JS.DoExpr(translateDec dec2 ret))]

  and translateRhsValBind vars (ML.PLAINValBind(pat, exp, valbind_opt)@@_) =
        (* var x = exp; valbind_opt *)
        JS.VarStmt(List.hd vars, translateExp exp) ::
        ?(translateRhsValBind(List.tl vars)) valbind_opt
    | translateRhsValBind vars (ML.RECValBind(valbind)@@_) =
        []

  and translateLhsValBind vars (ML.PLAINValBind(pat, exp, valbind_opt)@@_) =
        (* pat[[x]]; valbind_opt *)
        translatePat (List.hd vars, bindExpr) pat @
        ?(translateLhsValBind(List.tl vars)) valbind_opt
    | translateLhsValBind vars (ML.RECValBind(valbind)@@_) =
        []

  and translateRecValBind vars isRec (
        ML.PLAINValBind(pat, exp, valbind_opt)@@_
      ) =
        (if not isRec then
          []
        else case asSimplePat pat of
          SOME vid =>
            (* var vid = exp *)
            JS.VarStmt(JSTranslateId.translateVId' vid, translateExp exp) :: []
        | NONE =>
          let
            val var = List.hd vars
          in
            (* var x = exp; pat[[x]] *)
            JS.VarStmt(var, translateExp exp) ::
            translatePat (var, bindExpr) pat
          end
        ) @ ?(translateRecValBind (List.tl vars) isRec) valbind_opt
    | translateRecValBind vars isRec (ML.RECValBind(valbind)@@_) =
        translateRecValBind vars true valbind

  and translateRhsExBind vars (ML.NEWExBind(_, vid@@_, NONE, exbind_opt)@@_) =
        (* var x = new _JS.Error("vid") *)
        JS.VarStmt(List.hd vars,
          JS.NewExpr(errorExpr, [JS.StringExpr(VId.toString vid)])
        ) :: ?(translateRhsExBind(List.tl vars)) exbind_opt
    | translateRhsExBind vars (
        ML.NEWExBind(_, vid@@_, SOME(_@@A), exbind_opt)@@_
      ) =
      let
        val var  = List.hd vars
        val var' = JSSyntax.inventVar()
        val plain =
            case asTupleType(A.get(A.elab A)) of PLAIN => true | _ => false
      in
        (* x.prototype = new _JS.Error("vid");
         * function x(x') {
         *   var x' = _SML._tuplifyArgs(arguments);  // if not plain
         *   if (!(this instanceof x)) return new x(x');
         *   this.of = x';
         * }
         *)
        JS.AsnStmt(
          JS.DotExpr(JS.VarExpr(var), "prototype"),
          JS.NewExpr(errorExpr, [JS.StringExpr(VId.toString vid)])
        ) ::
        JS.FunStmt(var, if plain then [var'] else [],
          (if plain then [] else
            JS.VarStmt(var',
              JS.CallExpr(tuplifyArgsExpr, [JS.VarExpr("arguments")])
            ) :: []
          ) @
          JS.IfStmt(
            JS.UnExpr(JS.NotOp,
              JS.BinExpr(JS.InstOp, JS.ThisExpr, JS.VarExpr(var))
            ),
            JS.RetStmt(JS.NewExpr(JS.VarExpr(var), [JS.VarExpr(var')])),
            NONE
          ) ::
          JS.AsnStmt(JS.DotExpr(JS.ThisExpr, "of"), JS.VarExpr(var')) :: []
        ) :: ?(translateRhsExBind(List.tl vars)) exbind_opt
      end
    | translateRhsExBind vars (
        ML.EQUALExBind(_, vid, _, longvid, exbind_opt)@@_
      ) =
        (* var x = longvid; exbind_opt *)
        JS.VarStmt(List.hd vars, JSTranslateId.translateLongVId longvid) ::
        ?(translateRhsExBind(List.tl vars)) exbind_opt

  and translateLhsExBind vars (ML.NEWExBind(_, vid@@_, _, exbind_opt)@@_) =
        (* var vid = x; exbind_opt *)
        JS.VarStmt(JSTranslateId.translateVId' vid, JS.VarExpr(List.hd vars)) ::
        ?(translateLhsExBind(List.tl vars)) exbind_opt
    | translateLhsExBind vars (ML.EQUALExBind(_, vid@@_, _, _, exbind_opt)@@_) =
        (* var vid = x; exbind_opt *)
        JS.VarStmt(JSTranslateId.translateVId' vid, JS.VarExpr(List.hd vars)) ::
        ?(translateLhsExBind(List.tl vars)) exbind_opt


  (* Patterns *)

  and translateAtPat (var, exn) (ML.WILDCARDAtPat@@_) =
        []
    | translateAtPat (var, exn) (ML.SCONAtPat(scon)@@_) =
        (* if (x !== scon) throw exn *)
        JS.IfStmt(
          JS.BinExpr(JS.NeqOp,
            JS.VarExpr(var), JSTranslateSCon.translateSCon scon
          ),
          JS.ThrowStmt(exn),
          NONE
        ) :: []
    | translateAtPat (var, exn) (ML.IDAtPat(_, longvid as longvid'@@A)@@_) =
      let
        val vid = LongVId.toId longvid'
      in
        if #2(A.get(A.elab A)) = IdStatus.v then
          (* var vid = x *)
          JS.VarStmt(JSTranslateId.translateVId' vid, JS.VarExpr(var)) :: []
        else
          (* if (x !== longvid) throw exn *)
          JS.IfStmt(
            JS.BinExpr(JS.NeqOp,
              JS.VarExpr(var), JSTranslateId.translateLongVId longvid
            ),
            JS.ThrowStmt(exn),
            NONE
          ) :: []
      end
    | translateAtPat (var, exn) (ML.RECORDAtPat(patrow_opt)@@_) =
        ?(translatePatRow (var, exn)) patrow_opt
    | translateAtPat (var, exn) (ML.PARAtPat(pat)@@_) =
        translatePat (var, exn) pat

  and translatePatRow (var, exn) (ML.DOTSPatRow@@_) =
        []
    | translatePatRow (var, exn) (ML.FIELDPatRow(lab, pat, patrow_opt)@@_) =
      (case asSimplePat pat of
        SOME vid =>
          (* var vid = x.lab; patrow_opt *)
          JS.VarStmt(JSTranslateId.translateVId' vid,
            JS.DotExpr(JS.VarExpr(var), JSTranslateId.translateLab lab)
          ) :: ?(translatePatRow (var, exn)) patrow_opt
      | NONE =>
        let
          val var' = JS.inventVar()
        in
          (* var x' = x.lab; pat[[x']]; patrow_opt *)
          JS.VarStmt(var',
            JS.DotExpr(JS.VarExpr(var), JSTranslateId.translateLab lab)
          ) ::
          translatePat (var', exn) pat @
          ?(translatePatRow (var, exn)) patrow_opt
        end
      )

  and translatePat (var, exn) (ML.ATPat(atpat)@@_) =
        translateAtPat (var, exn) atpat
    | translatePat (var, exn) (ML.CONPat(_, longvid as longvid'@@A, atpat)@@_) =
      let
        val vid = LongVId.toId longvid'
        val is = #2(A.get(A.elab A))
      in
        (* if (test) { var x' = x.vid; atpat[[x']] } else throw exn *)
        JS.IfStmt(
          (case is of
            IdStatus.v =>
              raise Fail "JSTranslateCore.translatePat: value longvid"
          | IdStatus.c =>
              (* typeof x === "object" && 'vid' in x *)
              JS.BinExpr(JS.AndOp,
                JS.BinExpr(JS.EqOp,
                  JS.UnExpr(JS.TypOp, JS.VarExpr(var)), JS.StringExpr("object")
                ),
                JS.BinExpr(JS.InOp,
                  JS.StringExpr(VId.toString vid), JS.VarExpr(var)
                )
              )
          | IdStatus.e =>
              (* x instanceof longvid *)
              JS.BinExpr(JS.InstOp,
                JS.VarExpr(var), JSTranslateId.translateLongVId longvid
              )
          ),
          JS.BlockStmt(
            case asSimpleAtPat atpat of
              SOME vid' =>
                JS.VarStmt(JSTranslateId.translateVId' vid',
                  JS.DotExpr(JS.VarExpr(var), VId.toString vid)
                ) :: []
            | NONE =>
              let
                val var' = JS.inventVar()
              in
                JS.VarStmt(var',
                  JS.DotExpr(
                    JS.VarExpr(var),
                    if is = IdStatus.c then VId.toString vid else "of"
                  )
                ) :: translateAtPat (var', exn) atpat
              end
          ),
          SOME(JS.ThrowStmt(exn))
        ) :: []
      end
    | translatePat (var, exn) (ML.COLONPat(pat, ty)@@_) =
        translatePat (var, exn) pat
    | translatePat (var, exn) (ML.ASPat(_, vid, ty_opt, pat)@@_) =
        (* var vid = x; pat[[x]] *)
        JS.VarStmt(JSTranslateId.translateVId vid, JS.VarExpr(var)) ::
        translatePat (var, exn) pat
end;
