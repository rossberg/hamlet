(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules elaboration
 *
 * Definition, Sections 5.7 and 3.5
 * + RFC: Local modules
 *)

signature ELAB_MODULE =
sig
    (* Import types *)

    type StrDec  = GrammarModule.StrDec
    type TopDec  = GrammarModule.TopDec
    type Env     = StaticObjectsCore.Env
    type Context = StaticObjectsCore.Context
    type Basis   = StaticObjectsModule.Basis

    (* Export *)

    val elabStrDec : Context * StrDec -> Env
    val elabTopDec : Basis * TopDec -> Basis
end;
