(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic restrictions for modules
 *
 * Definition, Section 3.5
 *)

signature SYNTACTIC_RESTRICTIONS_MODULE =
sig
  (* Import *)

  type Basis  = BindingObjectsModule.Basis
  type TopDec = SyntaxModule.TopDec

  (* Export *)

  val checkTopDec : Basis * TopDec -> Basis
end;
