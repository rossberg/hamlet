(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML programs
 *
 * Definition, Section 8
 *
 * Note:
 *     State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     of modules and core can be avoided. Note that the state therefore
 *     never is returned.
 *)

structure Program : PROGRAM =
struct
    (* Import *)

    open GrammarProgram
    open Basis
    type State = EvalModule.State


    (* Helpers for output *)

    val width = 79

    fun printException(s, e) =
	( TextIO.output(TextIO.stdOut, "Uncaught exception: ")
	; PrettyPrint.output(TextIO.stdOut, PPVal.ppExVal(s, e), width)
	; TextIO.output1(TextIO.stdOut, #"\n")
	; TextIO.flushOut TextIO.stdOut
	)

    fun printStaticBasis B_STAT =
	( PrettyPrint.output(TextIO.stdOut, PPStaticBasis.ppBasis B_STAT,
			     width)
	; TextIO.flushOut TextIO.stdOut
	)

    fun printDynamicBasis(s, B_DYN) =
	( PrettyPrint.output(TextIO.stdOut, PPDynamicBasis.ppBasis(s, B_DYN),
			     width)
	; TextIO.flushOut TextIO.stdOut
	)

    fun printBasis(s, B) =
	( PrettyPrint.output(TextIO.stdOut, PPBasis.ppBasis(s, B), width)
	; TextIO.flushOut TextIO.stdOut
	)


    (* Helpers for basis modification *)

    val oplus = Basis.oplus

    infix oplus


    (* Inference rules [Section 8] *)

    fun execProgram echo (s,B, Program(I, topdec, program_opt)) =
	(* [Rules 187 to 189] *)
	let
	    val B_STAT1 = ElabModule.elabTopDec(Basis.B_STATof B, topdec)
	    val B_DYN1  = EvalModule.evalTopDec(s,Basis.B_DYNof B, topdec)
	    (* [Rule 189] *)
	    val _       = if echo then printBasis(!s, (B_STAT1,B_DYN1)) else ()
	    val B'      = B oplus (B_STAT1,B_DYN1)
	    val B''     = case program_opt
			    of NONE         => B'
			     | SOME program => execProgram echo (s,B', program)
	in
	    B''
	end
	handle Error.Error =>
	       (* [Rule 187] *)
	       let
		   val B' = case program_opt
			      of NONE         => B
			       | SOME program => execProgram echo (s,B, program)
	       in
		   B'
	       end

	     | DynamicObjectsCore.Pack e =>
	       (* [Rule 188] *)
	       let
		   val _  = printException(!s, e)
		   val B' = case program_opt
			      of NONE         => B
			       | SOME program => execProgram echo (s,B, program)
	       in
		   B'
	       end


    (* Elaboration only *)

    fun elabProgram echo (B_STAT, Program(I, topdec, program_opt)) =
	let
	    val B_STAT1  = ElabModule.elabTopDec(B_STAT, topdec)
	    val  _       = if echo then printStaticBasis B_STAT1 else ()
	    val B_STAT'  = StaticBasis.plus(B_STAT, B_STAT1)
	    val B_STAT'' = case program_opt
			     of NONE         => B_STAT'
			      | SOME program =>
				   elabProgram echo (B_STAT', program)
	in
	    B_STAT''
	end
	handle Error.Error =>
	       B_STAT


    (* Evaluation only *)

    fun evalProgram echo (s,B_DYN, Program(I, topdec, program_opt)) =
	let
	    val B_DYN1  = EvalModule.evalTopDec(s,B_DYN, topdec)
	    val  _      = if echo then printDynamicBasis(!s, B_DYN1) else ()
	    val B_DYN'  = DynamicBasis.plus(B_DYN, B_DYN1)
	    val B_DYN'' = case program_opt
			    of NONE         => B_DYN'
			     | SOME program =>
				  evalProgram echo (s,B_DYN', program)
	in
	    B_DYN''
	end
	handle Error.Error =>
	       (* Runtime error *)
	       let
		   val B_DYN' = case program_opt
				  of NONE         => B_DYN
				   | SOME program =>
					evalProgram echo (s,B_DYN, program)
	       in
		   B_DYN'
	       end

	     | DynamicObjectsCore.Pack e =>
	       let
		   val  _     = printException(!s, e)
		   val B_DYN' = case program_opt
				  of NONE         => B_DYN
				   | SOME program =>
					evalProgram echo (s,B_DYN, program)
	       in
		   B_DYN'
	       end
end;
