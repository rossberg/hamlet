(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML modules evaluation
 *
 * Definition, Section 7.3
 *
 * Notes: see EVAL_CORE-sig.sml
 *)

signature EVAL_MODULE =
sig
  (* Import types *)

  type TopDec = SyntaxModule.TopDec
  type Basis  = DynamicObjectsModule.Basis
  type State  = EvalCore.State

  (* Export *)

  val evalTopDec : (State ref * Basis) * TopDec -> Basis
end;
