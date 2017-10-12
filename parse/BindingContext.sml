(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML context for binding analysis
 *)

structure BindingContext : BINDING_CONTEXT =
struct
  (* Import *)

  open IdsCore
  open BindingObjectsCore


  (* Projections [Section 4.3] *)

  fun Uof(U, E) = U
  fun Eof(U, E) = E


  (* Modification [Section 4.3] *)

  infix plus plusU plusVE plusTE plusVEandTE plusE

  val op plus = BindingEnv.plus

  fun (U, E) plusU U'  = (TyVarSet.union(U, U'), E)
  fun (U, E) plusE E'  = (U, E plus E')
  fun (U, E) plusVE VE = (U, E plus BindingEnv.fromVE VE)
  fun (U, E) plusTE TE = (U, E plus BindingEnv.fromTE TE)
  fun (U, E) plusVEandTE (VE, TE) = (U, E plus BindingEnv.fromVEandTE (VE, TE))


  (* Application (lookup) [Section 4.3] *)

  fun findLongVId((U, E), longvid) = BindingEnv.findLongVId(E, longvid)
  fun findLongTyCon((U, E), longtycon) = BindingEnv.findLongTyCon(E, longtycon)
  fun findLongStrId((U, E), longstrid) = BindingEnv.findLongStrId(E, longstrid)
end;
