(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic analysis
 *)

signature PARSE =
sig
    (* Import *)

    type source  = Source.source
    type InfEnv  = Infix.InfEnv
    type Program = GrammarProgram.Program

    (* Export *)

    val parse : InfEnv * source * string option -> InfEnv * Program
end;
