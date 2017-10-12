(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of special constants into JavaScript.
 *)

signature JS_TRANSLATE_SCON =
sig
  (* Import *)

  type SCon = SyntaxCore.SCon

  type expr = JSSyntax.expr


  (* Export *)

  val translateSCon : SCon -> expr
end;
