(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic restrictions for the core
 *
 * Definition, Section 2.9
 *
 * Notes:
 * - The "syntactic restrictions" defined in the Definition are not purely
 *   syntactic. E.g. the restriction that valbinds may not bind the same vid
 *   twice [2nd bullet] cannot be checked without proper binding analysis
 *   that computes identifier status. For this reason we maintain binding
 *   environments.
 * - Also, checking of type variable shadowing [last bullet] is a global
 *   check dependent on context. Moreover, it requires the transformation from
 *   Section 4.6 to be done first.
 * - The Definition contains a bug -- an important syntactic restriction
 *   is missing:
 *     "Any tyvar occuring on the right side of a typbind or datbind of the
 *      form tyvarseq tycon = ... must occur in tyvarseq."
 *)

signature SYNTACTIC_RESTRICTIONS_CORE =
sig
  (* Import *)

  type VId         = IdsCore.VId
  type TyVar       = SyntaxCore.TyVar
  type 'a VIdMap   = 'a IdsCore.VIdMap
  type 'a TyConMap = 'a IdsCore.TyConMap
  type 'a StrIdMap = 'a IdsCore.StrIdMap

  type Dec         = SyntaxCore.Dec
  type Ty          = SyntaxCore.Ty
  type 'a seq      = 'a SyntaxCore.seq

  type TyVarSet    = BindingObjectsCore.TyVarSet
  type Env         = BindingObjectsCore.Env
  type Context     = BindingObjectsCore.Context


  (* Export *)

  val checkDec          : Context * Dec -> Env
  val checkTy           : Context * Ty -> TyVarSet
  val checkTyVarseq     : TyVar seq -> TyVarSet

  val isValidBindVId    : VId -> bool
  val isValidConBindVId : VId -> bool
end;
