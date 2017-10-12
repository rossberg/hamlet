(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of programs into JavaScript.
 *)

structure JSTranslateProgram : JS_TRANSLATE_PROGRAM =
struct
  (* Import *)

  type Program = SyntaxProgram.Program
  type stmt    = JSSyntax.stmt

  structure ML = SyntaxProgram
  structure A  = AnnotationProgram
  structure JS = JSSyntax

  datatype phrase = datatype Annotation.phrase


  (* Helpers *)

  fun ?translate(B, NONE)   = []
    | ?translate(B, SOME x) = translate(B, x)


  (* Programs *)

  fun translateProgram(B, ML.Program(topdec as _@@A1, program_opt)@@_) =
      let
        val B1 = A.get(A.elab A1)
        val stmts1 = JSTranslateModule.translateTopDec topdec []
      in
        if StaticBasis.disjoint(B, B1) then () else
          Error.warning(A.loc(A.annotation topdec),
            "shadowing top-level declaration, will overwrite");
        (case List.rev stmts1 of
          JS.RetStmt _ :: _ =>
          (* If the translation of the topdec ends in a return statement, then
           * we have to wrap it into a function and funnel back the local
           * environment wrapped into an object to open.
           * This happens when topdec involves local shadowing of some form.
           *)
          let
            val var = JS.inventVar()
          in
            (* var x = (function() { topdec[[return { pack[[., B1]] }]] })();
             * open[[x:B1]]
             *)
            JS.VarStmt(var,
              JS.DoExpr(
                JSTranslateModule.translateTopDec topdec (
                  JS.RetStmt(JS.ObjExpr(
                    JSTranslateModule.packBasis NONE B1)
                  ) :: []
                )
              )
            ) :: JSTranslateModule.openBasis (JS.VarExpr(var)) B1
          end
        | _ => stmts1
        ) @ ?translateProgram(StaticBasis.plus(B, B1), program_opt)
      end
end;
