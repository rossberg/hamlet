(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of programs into JavaScript.
 *)

signature COMPILE_JS =
sig
  (* Import *)

  type Program     = SyntaxProgram.Program
  type StaticBasis = StaticObjectsModule.Basis

  (* Export *)

  val compileProgram : TextIO.outstream -> StaticBasis * Program -> StaticBasis
end;
