(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML program derived forms
 *
 * Definition, Appendix A
 *)

signature DERIVED_FORMS_PROGRAM =
sig
  (* Import *)

  type Exp     = SyntaxCore.Exp
  type TopDec  = SyntaxModule.TopDec
  type Program = SyntaxProgram.Program

  (* Programs [Figure 18] *)

  val TOPDECProgram : TopDec * Program option -> SyntaxProgram.Program'
  val EXPProgram    : Exp * Program option -> SyntaxProgram.Program'
end;
