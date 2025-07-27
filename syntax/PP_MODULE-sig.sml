(*
 * (c) Andreas Rossberg 2007-2025
 *
 * Printer for abstract module syntax
 *)

signature PP_MODULE =
sig
    type TopDec = SyntaxModule.TopDec

    val ppTopDec : TextIO.outstream * int * TopDec -> unit
end;
