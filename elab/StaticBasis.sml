(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML static basis and environments of modules
 *
 * Definition, Section 5.1
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: Local modules
 *)

structure StaticBasis :> STATIC_BASIS =
struct
    (* Import *)

    open IdsCore
    open IdsModule
    open StaticObjectsCore
    open StaticObjectsModule


    (* Calculation of type variable and type name sets [Section 4.2] *)

    fun tyvars (T,E)       = StaticEnv.tyvars E
    fun tynames (T,E)      = TyNameSet.union(T, StaticEnv.tynames E)
    fun undetermined (T,E) = StaticEnv.undetermined E


    (* Injection [Sections 4.3 and 5.1; RFC: Local modules] *)

    val empty = ( TyNameSet.empty, StaticEnv.empty )

    fun fromTandE(T,E) = ( T, E )
    fun fromC (T,U,E)  = ( T, E )


    (* Projections [Sections 4.3 and 5.1] *)

    fun Tof (T,E) = T
    fun Cof (T,E) = (T, TyVarSet.empty, E)


    (* Modifications [Sections 4.3 and 5.1] *)

    infix plus plusT oplusG oplusE oplusSE

    fun (T,E) plus (T',E') =
	( TyNameSet.union(T,T')
	, StaticEnv.plus(E,E')
	)

    fun (T,E) plusT T' = ( TyNameSet.union(T,T'), E )

    fun (T,E) oplusE E' =
	( TyNameSet.union(T, StaticEnv.tynames E')
	, StaticEnv.plus(E,E')
	)

    fun (T,E) oplusSE SE =
	( TyNameSet.union(T, StaticEnv.tynamesSE SE)
	, StaticEnv.plusSE(E,SE)
	)

    fun (T,E) oplusG G =
	( TyNameSet.union(T, StaticEnv.tynamesG G)
	, StaticEnv.plusG(E,G)
	)

    (* Application (lookup) [Sections 5.1 and 4.3] *)

    fun findStrId((T,E), strid) = StaticEnv.findStrId(E, strid)
    fun findSigId((T,E), sigid) = case StaticEnv.findSigId(E, sigid)
				      of SOME(Sig Sigma) => SOME Sigma
				       | _               => NONE

    fun findLongTyCon((T,E), longtycon) =
	StaticEnv.findLongTyCon(E, longtycon)
    fun findLongStrId((T,E), longstrid) =
	StaticEnv.findLongStrId(E, longstrid)
    (* [RFC: Nested signatures] *)
    fun findLongSigId((T,E), longsigid) =
	case StaticEnv.findLongSigId(E, longsigid)
	  of SOME(Sig Sigma) => SOME Sigma
	   | _               => NONE


    (* Conversion to binding basis *)

    fun toBindingIs(IdStatus is) = is
      | toBindingIs(TyName t)    = IdStatus.c

    fun toBindingValEnv VE = VIdMap.map (fn(sigma,vs) => toBindingIs vs) VE
    fun toBindingTyEnv  TE = TyConMap.map (fn(theta,VE) => toBindingValEnv VE) TE
    fun toBindingStrEnv SE = StrIdMap.map toBindingMod SE
    (* [RFC: Nested signatures] *)
    and toBindingSigEnv G = SigIdMap.map toBindingSig G
    and toBindingEnv(Env(G, SE,TE,VE)) =
	BindingObjectsCore.Env(toBindingSigEnv G, toBindingStrEnv SE,
			       toBindingTyEnv TE, toBindingValEnv VE)

    (* [RFC: Higher-order functors] *)
    and toBindingMod(Struct E) =
	    BindingObjectsCore.Struct(toBindingEnv E)
      | toBindingMod(Functor(Fct(T,(M,(T',M'))))) =
	    BindingObjectsCore.Functor(BindingObjectsModule.Fct(toBindingMod M))
      | toBindingMod(Functor _) =
	    raise Fail "StaticBasis.toBindingMod: invalid functor"

    (* [RFC: Nested signatures] *)
    and toBindingSig(Sig(T,M)) =
	    BindingObjectsModule.Sig(toBindingMod M)
      | toBindingSig _ =
	    raise Fail "StaticBasis.toBindingSig: invalid signature"

    fun toBindingBasis (T,E) = toBindingEnv E
end;
