(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML initial dynamic basis
 *
 * Definition, Appendix D
 * + RFC: Semantic fixes
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *
 * Note:
 *     The Definition does not specify what the initial state has to contain.
 *     This is a bug as it must at least contain the exception names Match
 *     and Bind. We put the state associated with the initial basis in
 *     here, too.
 *)

structure InitialDynamicBasis : INITIAL_DYNAMIC_BASIS =
struct
    (* Import *)

    type Basis = DynamicObjectsModule.Basis
    type State = DynamicObjectsCore.State


    (* Enviornments [Appendix D; RFC: Higher-order functors;
     *                           RFC: Nested signatures] *)

    val E0 = InitialDynamicEnv.E0
    val B0 = E0


    (* State [RFC: Semantic fixes] *)

    val s0 = InitialDynamicEnv.s0
end;
