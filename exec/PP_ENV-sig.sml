(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the combined static/dynamic environment
 *)

signature PP_ENV =
sig
  type Env   = StaticObjectsCore.Env * DynamicObjectsCore.Env
  type State = DynamicObjectsCore.State

  val ppEnv : State * Env -> PrettyPrint.doc
end;
