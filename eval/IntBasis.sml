(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML interface basis
 *
 * Definition, Section 7.2
 *)

structure IntBasis :> INTBASIS =
struct
  (* Import types *)

  open IdsCore
  open IdsModule
  open DynamicObjectsModule


  (* Injections [Section 7.2] *)

  fun Inter(F, G, E) = (G, Inter.Inter E)


  (* Modifications [Sections 4.3 and 7.2] *)

  infix plusI

  fun (G, I) plusI I'  = (G, Inter.plus(I, I'))


  (* Application (lookup) [Sections 7.2 and 4.3] *)

  fun findSigId((G, I), sigid) = SigIdMap.find(G, sigid)

  fun findLongTyCon((G, I), longtycon) = Inter.findLongTyCon(I, longtycon)
end;
