(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML environment for binding analysis
 *)

structure BindingBasis : BINDING_BASIS =
struct
  (* Import *)

  open IdsCore
  open IdsModule
  open BindingObjectsCore
  open BindingObjectsModule


  (* Injection [Sections 4.3 and 5.1] *)

  val empty   = (FunIdMap.empty, SigIdMap.empty, BindingEnv.empty)
  fun fromE E = (FunIdMap.empty, SigIdMap.empty, E)
  fun fromF F = (F, SigIdMap.empty, BindingEnv.empty)
  fun fromG G = (FunIdMap.empty, G, BindingEnv.empty)


  (* Projections [Sections 4.3 and 5.1] *)

  fun Cof (F, G, E) = (TyVarSet.empty, E)


  (* Modifications [Sections 4.3 and 5.1] *)

  infix plus plusG plusF plusE plusSE

  fun (F, G, E) plus (F', G', E') =
        ( FunIdMap.unionWith #2 (F, F'),
          SigIdMap.unionWith #2 (G, G'),
          BindingEnv.plus(E, E') )

  fun (F, G, E) plusG G'  = (F, SigIdMap.unionWith #2 (G, G'), E)
  fun (F, G, E) plusF F'  = (FunIdMap.unionWith #2 (F, F'), G, E)
  fun (F, G, E) plusE E'  = (F, G, BindingEnv.plus(E, E'))
  fun (F, G, E) plusSE SE = (F, G, BindingEnv.plusSE(E, SE))


  (* Application (lookup) [Sections 5.1 and 4.3] *)

  fun findStrId((F, G, E), strid) = BindingEnv.findStrId(E, strid)
  fun findSigId((F, G, E), sigid) = SigIdMap.find(G, sigid)
  fun findFunId((F, G, E), funid) = FunIdMap.find(F, funid)
  fun findLongStrId((F, G, E), longstrid) =
        BindingEnv.findLongStrId(E, longstrid)
  fun findLongTyCon((F, G, E), longtycon) =
        BindingEnv.findLongTyCon(E, longtycon)
end;
