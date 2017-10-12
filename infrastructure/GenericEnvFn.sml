(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML generic core environment
 *
 * Definition, Sections 4.2, 4.3, 6.3 and 7.2
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

functor GenericEnvFn(
    type Env
    type ValStr
    type TyStr
    (* [RFC: Higher-order functors] *)
    type ModStr
    (* [RFC: Nested signatures] *)
    type SigStr
    val Env :   SigStr SigIdMap.map * ModStr StrIdMap.map *
		TyStr TyConMap.map * ValStr VIdMap.map -> Env
    val unEnv : Env -> SigStr SigIdMap.map * ModStr StrIdMap.map *
		       TyStr TyConMap.map * ValStr VIdMap.map
    val env :   ModStr -> Env option
) :> GENERIC_ENV
    where type Env	= Env
(**)where type ValStr	= ValStr
(**)where type TyStr	= TyStr
(**)where type ModStr	= ModStr
(**)where type SigStr	= SigStr
    =
struct
    (* Import *)

    open IdsCore
    open IdsModule

    type ValStr	= ValStr
    type TyStr	= TyStr
    type ModStr = ModStr
    type SigStr = SigStr
    type ValEnv	= ValStr VIdMap
    type TyEnv	= TyStr TyConMap
    type StrEnv	= ModStr StrIdMap
    type SigEnv	= SigStr SigIdMap
    type Env	= Env


    (* Injections [Section 4.3; RFC: Nested signatures] *)

    val emptyG             = SigIdMap.empty
    val emptySE            = StrIdMap.empty
    val emptyTE            = TyConMap.empty
    val emptyVE            = VIdMap.empty
    val empty              = Env(emptyG, emptySE, emptyTE, emptyVE)

    fun fromG G            = Env(G,      emptySE, emptyTE, emptyVE)
    fun fromSE SE          = Env(emptyG, SE,      emptyTE, emptyVE)
    fun fromTE TE          = Env(emptyG, emptySE, TE,      emptyVE)
    fun fromVE VE          = Env(emptyG, emptySE, emptyTE, VE     )
    fun fromVEandTE(VE,TE) = Env(emptyG, emptySE, TE,      VE     )


    (* Projections [Section 4.3; RFC: Nested signatures] *)

    fun Gof E  = #1(unEnv E)
    fun SEof E = #2(unEnv E)
    fun TEof E = #3(unEnv E)
    fun VEof E = #4(unEnv E)


    (* Modifications [Section 4.3; RFC: Nested signatures] *)

    infix plus plusVE plusTE plusSE plusG plusVEandTE

    fun E plus E' =
	Env( SigIdMap.unionWith #2 (Gof E, Gof E')
	   , StrIdMap.unionWith #2 (SEof E, SEof E')
	   , TyConMap.unionWith #2 (TEof E, TEof E')
	   , VIdMap.unionWith   #2 (VEof E, VEof E')
	   )

    fun E plusVE VE =
	Env(Gof E, SEof E, TEof E, VIdMap.unionWith #2 (VEof E,VE))
    fun E plusTE TE =
	Env(Gof E, SEof E, TyConMap.unionWith #2 (TEof E,TE), VEof E)
    fun E plusSE SE =
	Env(Gof E, StrIdMap.unionWith #2 (SEof E,SE), TEof E, VEof E)
    fun E plusG G =
	Env(SigIdMap.unionWith #2 (Gof E,G), SEof E, TEof E, VEof E)
    fun E plusVEandTE (VE,TE) =
	Env( Gof E
	   , SEof E
	   , TyConMap.unionWith #2 (TEof E,TE)
	   , VIdMap.unionWith   #2 (VEof E,VE)
	   )


    (* Application (lookup) [Section 4.3; RFC: Nested signatures] *)

    fun findVId  (E, vid)   = VIdMap.find(VEof E, vid)
    fun findTyCon(E, tycon) = TyConMap.find(TEof E, tycon)
    fun findStrId(E, strid) = StrIdMap.find(SEof E, strid)
    fun findSigId(E, sigid) = SigIdMap.find(Gof E, sigid)

    fun findLongX'(E, findX,      [],       x) = findX(E, x)
      | findLongX'(E, findX, strid::strids, x) =
	    Option.mapPartial (fn E => findLongX'(E, findX, strids, x))
			      (Option.mapPartial env (findStrId(E, strid)))

    fun findLongX (explodeLongX, findX) (E, longX) =
	let
	    val (strids,x) = explodeLongX longX
	in
	    findLongX'(E, findX, strids, x)
	end

    fun findLongVId   x = findLongX (LongVId.explode,   findVId) x
    fun findLongTyCon x = findLongX (LongTyCon.explode, findTyCon) x
    fun findLongStrId x = findLongX (LongStrId.explode, findStrId) x
    fun findLongSigId x = findLongX (LongSigId.explode, findSigId) x


    (* Disjointness *)

    fun disjoint(E1, E2) =
	    SigIdMap.disjoint(Gof E1, Gof E2) andalso
	    StrIdMap.disjoint(SEof E1, SEof E2) andalso
	    TyConMap.disjoint(TEof E1, TEof E2) andalso
	    VIdMap.disjoint(VEof E1, VEof E2)
end;
