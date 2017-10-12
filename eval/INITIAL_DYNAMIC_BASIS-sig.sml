(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML initial dynamic basis
 *
 * Definition, Appendix D
 *
 * Note: see INITIAL_DYNAMIC_ENV-sig.sml
 *)

signature INITIAL_DYNAMIC_BASIS =
sig
  (* Import *)

  type Basis = DynamicObjectsModule.Basis
  type State = DynamicObjectsCore.State


  (* Export *)

  val B0 : Basis
  val s0 : State
end;
