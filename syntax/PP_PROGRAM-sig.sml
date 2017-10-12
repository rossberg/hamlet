(*
 * (c) Andreas Rossberg 2007
 *
 * Printer for abstract program grammar
 *)

signature PP_PROGRAM =
sig
    type Program = GrammarProgram.Program

    val ppProgram : TextIO.outstream * int * Program -> unit
end;
