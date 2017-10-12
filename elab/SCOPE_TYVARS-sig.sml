(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML scope of type variables
 *
 * Definition, Section 4.6
 *)

signature SCOPE_TYVARS =
sig
    (* Import *)

    type ValBind  = GrammarCore.ValBind
    type TyVarSet = TyVarSet.set

    (* Operation *)

    val unguardedTyVars : ValBind -> TyVarSet
end;
