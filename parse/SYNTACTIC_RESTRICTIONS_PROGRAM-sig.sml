(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic restrictions for programs
 *)

signature SYNTACTIC_RESTRICTIONS_PROGRAM =
sig
    (* Import *)

    type Basis		= SyntacticRestrictionsModule.Basis
    type Program	= GrammarProgram.Program

    (* Export *)

    val checkProgram :	Basis * Program -> Basis
end;
