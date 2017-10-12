(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML contexts
 *
 * Definition, Sections 4.2, 4.3, 4.7, and 4.9
 *)

structure Context :> CONTEXT =
struct
  (* Import *)

  open IdsCore
  open StaticObjectsCore


  (* Projections [Section 4.3] *)

  fun Tof(T, U, E) = T
  fun Uof(T, U, E) = U
  fun Eof(T, U, E) = E


  (* Modification [Section 4.3] *)

  infix plusVE plusU oplusE oplusTE oplusVEandTE

  fun (T, U, E) plusVE VE = (T, U, StaticEnv.plusVE(E, VE))
  fun (T, U, E) plusU  U' = (T, TyVarSet.union(U, U'), E)

  fun (T, U, E) oplusE E' =
      (TyNameSet.union(T, StaticEnv.tynames E'), U, StaticEnv.plus(E, E'))

  fun (T, U, E) oplusTE TE =
      (TyNameSet.union(T, StaticEnv.tynamesTE TE), U, StaticEnv.plusTE(E, TE))

  fun (T, U, E) oplusVEandTE (VE, TE) =
      (TyNameSet.union(T, StaticEnv.tynamesTE TE), U,
        StaticEnv.plusVEandTE(E, (VE, TE)))


  (* Application (lookup) [Section 4.3] *)

  fun findVId((T, U, E), vid)     = StaticEnv.findVId(E, vid)
  fun findTyCon((T, U, E), tycon) = StaticEnv.findTyCon(E, tycon)
  fun findStrId((T, U, E), strid) = StaticEnv.findStrId(E, strid)

  fun findLongVId((T, U, E), longvid) =
        StaticEnv.findLongVId(E, longvid)
  fun findLongTyCon((T, U, E), longtycon) =
        StaticEnv.findLongTyCon(E, longtycon)
  fun findLongStrId((T, U, E), longstrid) =
        StaticEnv.findLongStrId(E, longstrid)


  (* Calculation of tyvars [Section 4.2] *)

  fun tyvars(T, U, E)       = TyVarSet.union(U, StaticEnv.tyvars E)
  fun undetermined(T, U, E) = StaticEnv.undetermined E
end;
