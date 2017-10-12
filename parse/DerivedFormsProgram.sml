(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML program derived forms
 *
 * Definition, Appendix A
 *)

structure DerivedFormsProgram :> DERIVED_FORMS_PROGRAM =
struct
  (* Import *)

  open SyntaxCore
  open SyntaxModule
  open SyntaxProgram
  open AnnotationProgram


  (* Programs [Figure 18] *)

  val TOPDECProgram = Program

  fun EXPProgram(exp, program_opt) =
      let
        val longvid  = LongVId.fromId(VId.fromString "it")@@left(exp)
        val atpat    = IDAtPat(NONE, longvid)@@left(exp)
        val pat      = ATPat(atpat)@@at(atpat)
        val valbind  = PLAINValBind(pat, exp, NONE)@@at(exp)
        val dec      = VALDec(Seq[]@@left(exp), valbind)@@at(exp)
        val strdec   = DECStrDec(dec)@@at(exp)
        val topdec   = STRDECTopDec(strdec, NONE)@@at(exp)
      in
        Program(topdec, program_opt)
      end
end;
