(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Printer for abstract program syntax
 *)

signature PP_PROGRAM =
sig
    type Program = SyntaxProgram.Program

    val ppProgram : TextIO.outstream * int * Program -> unit
end;
