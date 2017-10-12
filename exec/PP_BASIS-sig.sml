(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the combined basis
 *)

signature PP_BASIS =
sig
    type Basis = Basis.Basis
    type State = DynamicObjectsCore.State

    val ppBasis : State * Basis -> PrettyPrint.doc
end;
