(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the dynamic basis
 *)

signature PP_DYNAMIC_BASIS =
sig
    type Basis = DynamicObjectsModule.Basis
    type State = DynamicObjectsCore.State

    val ppBasis : State * Basis -> PrettyPrint.doc
end;
