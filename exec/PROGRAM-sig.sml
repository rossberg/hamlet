(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML programs
 *
 * Definition, Section 8
 *
 * Note:
 *     State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     of modules and core can be avoided. Note that the state therefore
 *     never is returned.
 *)

signature PROGRAM =
sig
    (* Import *)

    type Program      = GrammarProgram.Program
    type StaticBasis  = Basis.StaticBasis
    type DynamicBasis = Basis.DynamicBasis
    type Basis        = Basis.Basis
    type State        = State.State


    (* Export *)

    val execProgram : bool -> State ref * Basis * Program -> Basis
    val elabProgram : bool -> StaticBasis * Program -> StaticBasis
    val evalProgram : bool -> State ref * DynamicBasis * Program -> DynamicBasis
end;
