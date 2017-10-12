(*
 * (c) Andreas Rossberg 2007
 *
 * Printer for abstract module grammar
 *)

signature PP_MODULE =
sig
    type longSigId = IdsModule.longSigId
    type StrExp    = GrammarModule.StrExp
    type StrDec    = GrammarModule.StrDec
    type TopDec    = GrammarModule.TopDec

    val ppLongSigId : TextIO.outstream * int * longSigId -> unit
    val ppStrExp :    TextIO.outstream * int * StrExp -> unit
    val ppStrDec :    TextIO.outstream * int * StrDec -> unit
    val ppTopDec :    TextIO.outstream * int * TopDec -> unit
end;
