(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the dynamic environment
 *)

signature PP_DYNAMIC_ENV =
sig
    type Env   = DynamicObjectsCore.Env
    type Mod   = DynamicObjectsCore.Mod
    type State = DynamicObjectsCore.State

    val ppEnv : State * Env -> PrettyPrint.doc
    val ppMod : State * Mod -> PrettyPrint.doc
end;
