(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the static basis
 *)

signature PP_STATIC_BASIS =
sig
    type Basis  = StaticObjectsModule.Basis

    val ppBasis : Basis  -> PrettyPrint.doc
end;
