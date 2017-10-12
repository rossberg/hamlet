(*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML program evaluation
 *
 * Definition, Section 8
 *
 * Note:
 * - See alse EVAL_CORE-sig.sml
 * - Isolates the dynamic semantics aspect of rules 187-189. Not needed for
 *   executing programs interactively, as defined in Section 8, but useful
 *   when running programs in batch mode.
 *)

signature EVAL_PROGRAM =
sig
  (* Import types *)

  type Program = SyntaxProgram.Program
  type Basis   = DynamicObjectsModule.Basis
  type State   = EvalModule.State

  (* Export *)

  val evalProgram : (State ref * Basis) * Program -> Basis
end;
