(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML interfaces
 *
 * Definition, Section 7.2
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

structure Inter :> INTER =
struct
    (* Import *)

    open IdsCore
    open IdsModule
    open DynamicObjectsModule


    (* Inheritance *)

    structure GenericEnv = GenericEnvFn(type Env     = Int
					type ValStr  = ValIntStatus
					type TyStr   = ValInt
					type ModStr  = Int
					type SigStr  = exn
					fun env I    = SOME I
					val Env      = Int
					fun unEnv(Int I) = I)

    open DynamicObjectsCore


    (* Injections [Section 4.3; RFC: Nested signatures] *)

    val empty		= GenericEnv.empty

    val fromG		= GenericEnv.fromG
    val fromSI		= GenericEnv.fromSE
    val fromTI		= GenericEnv.fromTE
    val fromVI		= GenericEnv.fromVE
    val fromVIandTI	= GenericEnv.fromVEandTE


    (* Projections [Section 4.3; RFC: Nested signatures] *)

    val Gof		= GenericEnv.Gof
    val SIof		= GenericEnv.SEof
    val TIof		= GenericEnv.TEof
    val VIof		= GenericEnv.VEof


    (* Modification [Section 4.3; RFC: Nested signatures] *)

    val plus		= GenericEnv.plus
    val plusSI		= GenericEnv.plusSE
    val plusG		= GenericEnv.plusG


    (* Application (lookup) [Section 4.3; RFC: Nested signatures] *)

    fun findLongTyCon(I, longtycon) = GenericEnv.findLongTyCon(I, longtycon)

    fun findLongSigId(I, longsigid) =
	case GenericEnv.findLongSigId(I, longsigid)
	  of SOME(Sig I) => SOME I
	   | _           => NONE


    (* Extracting interfaces from environments [Section 7.2; RFC: Views;
     *                                          RFC: Higher-order functors] *)

    (* [RFC: Views] *)
    fun InterVE VE = VIdMap.map (fn (v,IdStatus is) =>
				    DynamicObjectsModule.IdStatus is
				  | (v,Vals(v',vid')) =>
				    DynamicObjectsModule.f) VE
    fun InterTE TE = TyConMap.map (fn VE => InterVE VE) TE
    fun InterSE SE = StrIdMap.map (fn M => InterM M) SE

    and Inter(Env(G,SE,TE,VE)) = Int(G, InterSE SE, InterTE TE, InterVE VE)

    (* [RFC: Higher-order functors] *)
    and InterM(Struct E)  = Inter E
      | InterM(Functor _) = empty


    (* Cutting down environments [Section 7.2; RFC: Views;
     *                                         RFC: Higher-order functors;
     *                                         RFC: Nested signatures] *)

    (* [RFC: Views] *)
    fun cutdownVs(vs, DynamicObjectsModule.IdStatus is') = IdStatus is'
      | cutdownVs(vs, DynamicObjectsModule.f)            = vs

    fun cutdownVE(VE, VI) =
	VIdMap.foldli
	    (fn(vid, vis, VE') =>
		case VIdMap.find(VE, vid)
		  of SOME(v,vs) => VIdMap.insert(VE', vid, (v,cutdownVs(vs,vis)))
		   | NONE       => VE'
	    ) VIdMap.empty VI

    fun cutdownTE(TE, TI) =
	TyConMap.foldli
	    (fn(tycon, VI', TE') =>
		case TyConMap.find(TE, tycon)
		  of SOME VE' => TyConMap.insert(TE', tycon, cutdownVE(VE',VI'))
		   | NONE     => TE'
	    ) TyConMap.empty TI

    fun cutdownSE(SE, SI) =
	StrIdMap.foldli
	    (fn(strid, I, SE') =>
		case StrIdMap.find(SE, strid)
		  of SOME M =>
		       (* [RFC: Higher-order functors] *)
		       StrIdMap.insert(SE', strid, cutdown(M,I))
		   | NONE => SE'
	    ) StrIdMap.empty SI

    (* [RFC: Nested signatures] *)
    and cutdownE(Env(G,SE,TE,VE), Int(G',SI,TI,VI)) =
	    Env(G, cutdownSE(SE, SI), cutdownTE(TE, TI), cutdownVE(VE, VI))

    (* [RFC: Higher-order functors] *)
    and cutdown(Struct E, I) =
	    Struct(cutdownE(E, I))
      | cutdown(Functor(Fct((strid,I'), (strexp,IC), B)), I) =
	    Functor(Fct((strid,I'), (strexp, SOME I), B))
      | cutdown(Functor _, I) =
	    raise Fail "Inter.cutdown: invalid functor"

    (* [RFC: Higher-order functors] *)
    fun cutdownIC(M, NONE)   = M
      | cutdownIC(M, SOME I) = cutdown(M, I)
end;
