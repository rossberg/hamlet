(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML dynamic basis and environments of modules
 *
 * Definition, Section 7.2
 *)

structure DynamicBasis :> DYNAMIC_BASIS =
struct
  (* Import *)

  open IdsCore
  open IdsModule
  open DynamicObjectsCore
  open DynamicObjectsModule


  (* Injections [Sections 4.3 and 7.2] *)

  val empty = (FunIdMap.empty, SigIdMap.empty, DynamicEnv.empty)

  fun fromE E = (FunIdMap.empty, SigIdMap.empty, E)
  fun fromF F = (F, SigIdMap.empty, DynamicEnv.empty)
  fun fromG G = (FunIdMap.empty, G, DynamicEnv.empty)


  (* Projections [Sections 4.3 and 7.2] *)

  fun Eof(F, G, E) = E


  (* Modifications [Sections 4.3 and 7.2] *)

  infix plus plusG plusF plusE plusSE IBplusI

  fun (F, G, E) plus (F', G', E') =
        ( FunIdMap.unionWith #2 (F, F'),
          SigIdMap.unionWith #2 (G, G'),
          DynamicEnv.plus(E, E')
        )

  fun (F, G, E) plusG G'  = (F, SigIdMap.unionWith #2 (G, G'), E)
  fun (F, G, E) plusF F'  = (FunIdMap.unionWith #2 (F, F'), G, E)
  fun (F, G, E) plusE E'  = (F, G, DynamicEnv.plus(E, E'))
  fun (F, G, E) plusSE SE = (F, G, DynamicEnv.plusSE(E, SE))


  (* Application (lookup) [Sections 7.2 and 4.3] *)

  fun findStrId((F, G, E), strid) = DynamicEnv.findStrId(E, strid)
  fun findSigId((F, G, E), sigid) = SigIdMap.find(G, sigid)
  fun findFunId((F, G, E), funid) = FunIdMap.find(F, funid)

  fun findLongStrId((F, G, E), longstrid) =
      DynamicEnv.findLongStrId(E, longstrid)
  fun findLongTyCon((F, G, E), longtycon) =
      DynamicEnv.findLongTyCon(E, longtycon)
end;
