(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of programs into JavaScript.
 *)

signature JS_TRANSLATE_PROGRAM =
sig
  (* Import *)

  type Program = SyntaxProgram.Program

  type stmt = JSSyntax.stmt

  (* Export *)

  val translateProgram : StaticBasis.Basis * Program -> stmt list
end;
