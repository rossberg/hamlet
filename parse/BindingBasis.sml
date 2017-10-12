(*
 * (c) Andreas Rossberg 1999-2007
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


    (* Injection [Sections 4.3 and 5.1; RFC: Higher-order functors;
     *                                  RFC: Nested signatures;
     *                                  RFC: Local modules] *)

    val empty   = BindingEnv.empty
    fun fromE E = E
    val fromSE  = BindingEnv.fromSE
    val fromG   = BindingEnv.fromG
    fun fromC (U,E) = E


    (* Projections [Sections 4.3 and 5.1; RFC: Higher-order functors;
     *                                    RFC: Nested signatures] *)

    fun Cof E = (TyVarSet.empty, E)


    (* Modifications [Sections 4.3 and 5.1; RFC: Higher-order functors;
     *                                      RFC: Nested signatures] *)

    val plus   = BindingEnv.plus
    val plusE  = BindingEnv.plus
    val plusSE = BindingEnv.plusSE
    val plusG  = BindingEnv.plusG


    (* Application (lookup) [Sections 5.1 and 4.3; RFC: Higher-order functors;
     *                                             RFC: Nested signatures] *)

    fun findStrId(E, strid) = BindingEnv.findStrId(E, strid)
    fun findSigId(E, sigid) = case BindingEnv.findSigId(E, sigid)
				of SOME(Sig M) => SOME M
				 | _ => NONE

    fun findLongTyCon(E, longtycon) = BindingEnv.findLongTyCon(E, longtycon)
    fun findLongStrId(E, longstrid) = BindingEnv.findLongStrId(E, longstrid)
    fun findLongSigId(E, longsigid) = case BindingEnv.findLongSigId(E,longsigid)
					of SOME(Sig M) => SOME M
					 | _ => NONE
end;
