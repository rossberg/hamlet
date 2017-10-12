(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML abstract program grammar
 *
 * Definition, Section 8
 *)


functor GrammarProgramFn(type Info
			 structure Module : GRAMMAR_MODULE
			) : GRAMMAR_PROGRAM =
struct
    (* Import *)

    structure Module = Module
    type      Info   = Info

    open Module


    (* Programs *)

    datatype Program = Program of Info * TopDec * Program option


    (* Extracting the info field *)

    fun infoProgram(Program(I,_,_)) = I
end;
