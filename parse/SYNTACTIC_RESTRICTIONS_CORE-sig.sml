(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic restrictions for the core
 *
 * Definition, Section 2.9
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Notes:
 *   - The "syntactic restrictions" defined in the Definition are not purely
 *     syntactic. E.g. the restriction that valbinds may not bind the same vid
 *     twice [2nd bullet] cannot be checked without proper binding analysis,
 *     to compute identifier status.
 *   - Also, checking of type variable shadowing [last bullet] is a global
 *     check dependent on context. Moreover, it requires the transformation from
 *     Section 4.6 to be done first.
 *   - The Definition contains a bug -- an important syntactic restriction
 *     is missing:
 *       "Any tyvar occuring on the right side of a typbind or datbind of the
 *        form tyvarseq tycon = ... must occur in tyvarseq."
 *)

signature SYNTACTIC_RESTRICTIONS_CORE =
sig
    (* Import *)

    type VId			= IdsCore.VId
    type 'a VIdMap		= 'a IdsCore.VIdMap
    type 'a TyConMap		= 'a IdsCore.TyConMap
    type 'a StrIdMap		= 'a IdsCore.StrIdMap

    type AtExp			= GrammarCore.AtExp
    type Dec			= GrammarCore.Dec
    type Ty			= GrammarCore.Ty
    type TyVarseq		= GrammarCore.TyVarseq
    type StrDec'		= GrammarCore.StrDec'

    type TyVarSet		= BindingObjectsCore.TyVarSet
    type Env			= BindingObjectsCore.Env
    type Context		= BindingObjectsCore.Context


    (* Recursive import *)

    structure SyntacticRestrictionsModule :
	sig val checkStrDec : (Context * StrDec' -> Env) ref end


    (* Export *)

    val checkAtExp :		Context * AtExp -> unit
    val checkDec :		Context * Dec -> Env
    val checkTy :		Ty -> TyVarSet
    val checkTyVarseq :		TyVarseq -> TyVarSet

    val validBindVId :		VId -> bool
    val validConBindVId :	VId -> bool
end;
