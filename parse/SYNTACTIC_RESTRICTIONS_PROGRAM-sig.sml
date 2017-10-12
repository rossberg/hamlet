(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic restrictions for programs
 *)

signature SYNTACTIC_RESTRICTIONS_PROGRAM =
sig
  (* Import *)

  type Basis   = SyntacticRestrictionsModule.Basis
  type Program = SyntaxProgram.Program

  (* Export *)

  val checkProgram : Basis * Program -> Basis
end;
