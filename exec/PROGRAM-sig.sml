(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML programs
 *
 * Definition, Section 8
 *
 * Note: see EVAL_CORE-sig.sml
 *)

signature PROGRAM =
sig
  (* Import *)

  type Program      = SyntaxProgram.Program
  type StaticBasis  = Basis.StaticBasis
  type DynamicBasis = Basis.DynamicBasis
  type Basis        = Basis.Basis
  type State        = State.State


  (* Export *)

  val execProgram : bool -> (State ref * Basis) * Program -> Basis
  val elabProgram : bool -> StaticBasis * Program -> StaticBasis
  val evalProgram : bool -> (State ref * DynamicBasis) * Program -> DynamicBasis
end;
