(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML abstract program grammar
 *
 * Definition, Section 8
 *)

signature GRAMMAR_PROGRAM =
sig
    (* Import *)

    structure Module : GRAMMAR_MODULE

    type Info	= Module.Info

    type TopDec = Module.TopDec


    (* Programs *)

    datatype Program = Program of Info * TopDec * Program option


    (* Extracting the info field *)

    val infoProgram : Program -> Info
end;
