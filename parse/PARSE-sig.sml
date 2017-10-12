(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic analysis
 *)

signature PARSE =
sig
  (* Import *)

  type source  = Source.source
  type InfEnv  = Infix.InfEnv
  type Program = SyntaxProgram.Program


  (* Export *)

  val parse : InfEnv * source * string option -> InfEnv * Program
end;
