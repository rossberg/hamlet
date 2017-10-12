(*
 * (c) Andreas Rossberg 2013
 *
 * Printer for JavaScript syntax
 *)

signature PP_JS =
sig
  val ppVar  : JSSyntax.var -> PrettyPrint.doc
  val ppExpr : JSSyntax.expr -> PrettyPrint.doc
  val ppStmt : JSSyntax.stmt -> PrettyPrint.doc
end;
