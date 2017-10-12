(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML generic core environment
 *
 * Definition, Sections 4.2, 4.3, 6.3 and 7.2
 *)

functor GenericEnvFn(
  type Env
  type ValStr
  type TyStr
  val Env   : Env StrIdMap.map * TyStr TyConMap.map * ValStr VIdMap.map -> Env
  val unEnv : Env -> Env StrIdMap.map * TyStr TyConMap.map * ValStr VIdMap.map
) :>
GENERIC_ENV
  where type Env    = Env
    and type ValStr = ValStr
    and type TyStr  = TyStr =
struct
    (* Import *)

    open IdsCore

    type ValStr = ValStr
    type TyStr  = TyStr
    type ValEnv = ValStr VIdMap
    type TyEnv  = TyStr TyConMap
    type StrEnv = Env StrIdMap
    type Env    = Env


    (* Injections [Section 4.3] *)

    val emptySE             = StrIdMap.empty
    val emptyTE             = TyConMap.empty
    val emptyVE             = VIdMap.empty
    val empty               = Env(emptySE, emptyTE, emptyVE)

    fun fromSE SE           = Env(SE, emptyTE, emptyVE)
    fun fromTE TE           = Env(emptySE, TE, emptyVE)
    fun fromVE VE           = Env(emptySE, emptyTE, VE)
    fun fromVEandTE(VE, TE) = Env(emptySE, TE, VE)


    (* Projections [Section 4.3] *)

    fun SEof E = #1(unEnv E)
    fun TEof E = #2(unEnv E)
    fun VEof E = #3(unEnv E)


    (* Modifications [Section 4.3] *)

    infix plus plusVE plusTE plusSE plusVEandTE

    fun E plus E' =
        Env(
          StrIdMap.unionWith #2 (SEof E, SEof E'),
          TyConMap.unionWith #2 (TEof E, TEof E'),
          VIdMap.unionWith #2 (VEof E, VEof E')
        )

    fun E plusVE VE = Env(SEof E, TEof E, VIdMap.unionWith #2 (VEof E, VE))
    fun E plusTE TE = Env(SEof E, TyConMap.unionWith #2 (TEof E, TE), VEof E)
    fun E plusSE SE = Env(StrIdMap.unionWith #2 (SEof E, SE), TEof E, VEof E)
    fun E plusVEandTE (VE, TE) =
        Env(
          SEof E,
          TyConMap.unionWith #2 (TEof E, TE),
          VIdMap.unionWith #2 (VEof E, VE)
        )


    (* Application (lookup) [Section 4.3] *)

    fun findVId(E, vid)     = VIdMap.find(VEof E, vid)
    fun findTyCon(E, tycon) = TyConMap.find(TEof E, tycon)
    fun findStrId(E, strid) = StrIdMap.find(SEof E, strid)

    fun findLongX'(E, findX, [], x) =
          findX(E, x)
      | findLongX'(E, findX, strid::strids, x) =
          Option.mapPartial (fn E => findLongX'(E, findX, strids, x))
            (findStrId(E, strid))

    fun findLongX (explodeLongX, findX) (E, longX) =
        let
          val (strids, x) = explodeLongX longX
        in
          findLongX'(E, findX, strids, x)
        end

    fun findLongVId x   = findLongX (LongVId.explode,   findVId) x
    fun findLongTyCon x = findLongX (LongTyCon.explode, findTyCon) x
    fun findLongStrId x = findLongX (LongStrId.explode, findStrId) x


    (* Disjointness *)

    fun disjoint(E1, E2) =
          StrIdMap.disjoint(SEof E1, SEof E2) andalso
          TyConMap.disjoint(TEof E1, TEof E2) andalso
          VIdMap.disjoint(VEof E1, VEof E2)
end;
