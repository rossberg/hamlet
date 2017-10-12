(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML modules elaboration
 *
 * Definition, Sections 5.7 and 3.5
 *)

signature ELAB_MODULE =
sig
  (* Import types *)

  type TopDec = SyntaxModule.TopDec
  type Basis  = StaticObjectsModule.Basis

  (* Export *)

  val elabTopDec : Basis * TopDec -> Basis
end;
