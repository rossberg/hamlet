(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of values
 *)

signature PP_VAL =
sig
    (* Import *)

    type Val   = DynamicObjectsCore.Val
    type ExVal = DynamicObjectsCore.ExVal
    type Mod   = DynamicObjectsCore.Mod
    type State = DynamicObjectsCore.State

    (* Recursive import *)

    structure PPDynamicEnv :
	sig val ppMod : (State * Mod -> PrettyPrint.doc) ref end

    (* Export *)

    val ppVal :   State * Val -> PrettyPrint.doc
    val ppExVal : State * ExVal -> PrettyPrint.doc
end;
