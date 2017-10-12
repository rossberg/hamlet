(*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML program elaboration
 *
 * Definition, Section 8
 *
 * Note:
 *   Isolates the static semantics aspect of rules 187-189. Not needed for
 *   executing programs interactively, as defined in Section 8, but useful
 *   when implementing off-line compilation.
 *)

signature ELAB_PROGRAM =
sig
  (* Import types *)

  type Program = SyntaxProgram.Program
  type Basis   = StaticObjectsModule.Basis

  (* Export *)

  val elabProgram : Basis * Program -> Basis
end;
