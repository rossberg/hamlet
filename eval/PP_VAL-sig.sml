(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of values
 *)

signature PP_VAL =
sig
  type Val   = DynamicObjectsCore.Val
  type ExVal = DynamicObjectsCore.ExVal
  type State = DynamicObjectsCore.State

  val ppVal   : State * Val -> PrettyPrint.doc
  val ppExVal : State * ExVal -> PrettyPrint.doc
end;
