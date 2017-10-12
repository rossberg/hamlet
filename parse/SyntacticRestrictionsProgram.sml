(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic restrictions for programs
 *)

structure SyntacticRestrictionsProgram : SYNTACTIC_RESTRICTIONS_PROGRAM =
struct
    (* Import *)

    open GrammarProgram

    type Basis = SyntacticRestrictionsModule.Basis


    (* Operation *)

    fun checkProgram(B, Program(I, topdec, program_opt)) =
	let
	    val B1  = SyntacticRestrictionsModule.checkTopDec(B, topdec)
	    val B'  = BindingBasis.plus(B, B1)
	    val B'' = case program_opt
			of NONE         => B'
			 | SOME program => checkProgram(B', program)
	in
	    B''
	end
end;
