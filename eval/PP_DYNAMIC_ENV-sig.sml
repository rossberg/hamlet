(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the dynamic environment
 *)

signature PP_DYNAMIC_ENV =
sig
  type Env   = DynamicObjectsCore.Env
  type State = DynamicObjectsCore.State

  val ppEnv : State * Env -> PrettyPrint.doc
  val ppStr : State * Env -> PrettyPrint.doc
end;
