(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic restrictions for modules
 *
 * Definition, Section 3.5
 * + RFC: Local modules
 *)

signature SYNTACTIC_RESTRICTIONS_MODULE =
sig
    (* Import *)

    type StrDec		= GrammarModule.StrDec
    type TopDec		= GrammarModule.TopDec
    type Env		= BindingObjectsCore.Env
    type Context	= BindingObjectsCore.Context
    type Basis		= BindingObjectsModule.Basis

    (* Export *)

    val checkStrDec :	Context * StrDec -> Env
    val checkTopDec :	Basis * TopDec -> Basis
end;
