(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML program derived forms
 *
 * Definition, Appendix A
 * + RFC: Simplified recursive value bindings
 * + RFC: Nested signatures
 * + RFC: Local modules
 *)

structure DerivedFormsProgram :> DERIVED_FORMS_PROGRAM =
struct
    (* Import *)

    structure C  = GrammarCore
    structure M  = GrammarModule
    structure P  = GrammarProgram

    type Info    = GrammarProgram.Info

    type Exp     = GrammarCore.Exp
    type TopDec  = GrammarModule.TopDec
    type Program = GrammarProgram.Program


    (* Programs [Figure 18] *)

    fun TOPDECProgram(I, topdec, program_opt) =
	    P.Program(I, topdec, program_opt)

    fun EXPProgram(I, exp, program_opt) =
	let
	    val longvid = LongVId.fromId(VId.fromString "it")
	    val pat     = C.ATPat(I, C.IDAtPat(I, C.SANSOp, longvid))
	    val valbind = C.ValBind(I, pat, exp, NONE)
	    val dec     = C.VALDec(I, C.SANSRec, C.TyVarseq(I, []), valbind)
	    val topdec  = M.TopDec(I, dec)
	in
	    P.Program(I, topdec, program_opt)
	end
end;
