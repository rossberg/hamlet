(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules elaboration
 *
 * Definition, Sections 5.7 and 3.5
 *)

signature VALID_MODULE =
sig
    (* Import types *)

    type TopDec = SyntaxModule.TopDec
    type Basis  = StaticObjectsModule.Basis

    (* Export *)

    val validTopDec : Basis * TopDec -> Basis
end;
