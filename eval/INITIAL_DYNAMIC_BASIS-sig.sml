(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML initial dynamic basis
 *
 * Definition, Appendix D
 * + RFC: Semantic fixes
 *
 * Note:
 *     The Definition does not specify what the initial state has to contain.
 *     This is a bug as it must at least contain the exception names Match
 *     and Bind. We put the state associated with the initial basis in
 *     here, too.
 *)

signature INITIAL_DYNAMIC_BASIS =
sig
    (* Import *)

    type Basis = DynamicObjectsModule.Basis
    type State = DynamicObjectsCore.State


    (* Initial basis [Appendix D] *)

    val B0 : Basis

    (* State [RFC: Semantic fixes] *)

    val s0 : State
end;
