(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML initial static basis
 *
 * Definition, Appendices C and E
 *)

structure InitialStaticBasis : INITIAL_STATIC_BASIS =
struct
  (* Import *)

  type Basis = StaticObjectsModule.Basis


  (* Environments *)

  val T0 = InitialStaticEnv.T0
  val F0 = FunIdMap.empty
  val G0 = SigIdMap.empty
  val E0 = InitialStaticEnv.E0

  val B0 = (T0, F0, G0, E0)
end;
