(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML initial dynamic basis
 *
 * Definition, Appendix D
 *
 * Notes: see INITIAL_DYNAMIC_ENV-sig.sml
 *)

structure InitialDynamicBasis : INITIAL_DYNAMIC_BASIS =
struct
  (* Import *)

  type Basis = DynamicObjectsModule.Basis
  type State = DynamicObjectsCore.State


  (* Environments *)

  val F0 = FunIdMap.empty
  val G0 = SigIdMap.empty
  val E0 = InitialDynamicEnv.E0

  val B0 = (F0, G0, E0)


  (* Associated state *)

  val s0 = InitialDynamicEnv.s0
end;
