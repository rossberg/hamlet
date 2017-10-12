(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML dynamic basis and environments of modules
 *
 * Definition, Section 7.2
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

structure DynamicBasis :> DYNAMIC_BASIS =
struct
    (* Import *)

    open IdsCore
    open IdsModule
    open DynamicObjectsCore
    open DynamicObjectsModule


    (* Injections [Sections 4.3 and 7.2] *)

    val empty   = DynamicEnv.empty
    fun fromE E = E


    (* Projections [Sections 4.3 and 7.2] *)

    fun Eof E = E


    (* Modifications [Sections 4.3 and 7.2] *)

    val plus   = DynamicEnv.plus
    val plusE  = DynamicEnv.plus
    val plusSE = DynamicEnv.plusSE
    val plusG  = DynamicEnv.plusG


    (* Application (lookup) [Sections 7.2 and 4.3] *)

    fun findStrId(E, strid) = DynamicEnv.findStrId(E, strid)
    fun findSigId(E, sigid) = case DynamicEnv.findSigId(E, sigid)
				of SOME(Sig I) => SOME I
				 | _           => NONE
    fun findLongTyCon(E, longtycon) =
	DynamicEnv.findLongTyCon(E, longtycon)
    fun findLongStrId(E, longstrid) =
	DynamicEnv.findLongStrId(E, longstrid)
    fun findLongSigId(E, longsigid) =
	case DynamicEnv.findLongSigId(E, longsigid)
	  of SOME(Sig I) => SOME I
	   | _           => NONE
end;
