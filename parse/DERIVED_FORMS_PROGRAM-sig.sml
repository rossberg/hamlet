(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML program derived forms
 *
 * Definition, Appendix A
 *)

signature DERIVED_FORMS_PROGRAM =
sig
    (* Import *)

    type Info    = GrammarProgram.Info

    type Exp     = GrammarCore.Exp
    type TopDec  = GrammarModule.TopDec
    type Program = GrammarProgram.Program


    (* Programs [Figure 18] *)

    val TOPDECProgram :	Info * TopDec * Program option -> Program
    val EXPProgram :	Info *  Exp   * Program option -> Program
end;
